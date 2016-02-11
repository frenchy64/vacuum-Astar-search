(ns hw2-clj.core
  (:require [clojure.data.priority-map :refer [priority-map]]
            [clojure.pprint :refer [pprint]]))

; (defalias Position
;   "The state of a square on the game world"
;   '{:x Int
;     :y Int
;     :has-dirt? Bool
;     :has-vacuum? Bool})

; (defalias State 
;   "The state of a game world"
;   (Vec (Vec Position))

; (defalias Node
;   "A search node that contains the current world state"
;   '{:state (Vec (Vec Position))
;     :parent (Nilable Node)
;     :pos '{:x Int :y Int}
;     :cost Int
;     :dimensions '{:x Int :y Int}
;     :stats (Atom '{:search-cost Int
;                    :max-branching Int})})

; [-> Node]
(defn init-map 
  "An initial node with a 3x3 grid. Dirt is on the top row.
  Vacuum starts in the centre."
  ([] 
   ;; default to 3x3 grid
   (init-map 3 3))
  ([x y]
   ;; dirt on just top row
   (init-map x y
             (fn [i j]
               (= (inc j) y))))
  ([x y dirt-fn]
   (let [vacuum-pos {:x (int (/ x 2)) 
                     :y (int (/ y 2))}
         s (vec
             (for [i (range x)]
               (vec
                 (for [j (range y)]
                   {:x i
                    :y j
                    :has-dirt? (dirt-fn i j)
                    :has-vacuum? (= vacuum-pos {:x i :y j})}))))]
     {:state s
      :parent nil
      :pos vacuum-pos
      :move nil
      :cost 0
      :dimensions {:x x :y y}
      :stats (atom {:search-cost 0
                    :max-branching 0})})))

; [Node -> Bool]
(defn goal-state?
  "We're done if every position is clean"
  [s]
  {:pre [(map? s)]}
  (every? (fn [row]
            (every? (complement :has-dirt?) row))
          (:state s)))

; (defalias Move (U ':up ':down ':left ':right ':suck))

; [Node Move -> (Nilable Node)]
(defn apply-move
  "Returns nil if this is an illegal move, otherwise applies the move.
  Only sucks when there is dirt."
  [{:keys [pos state dimensions] :as g} move]
  (letfn [(update-common [m]
            (-> m
                (assoc :parent g)
                (update :cost inc)
                (assoc :move move)))]
    (case move
      (:suck) (when (get-in g [:state (:x pos) (:y pos) :has-dirt?])
                (-> g
                    update-common
                    (assoc-in [:state (:x pos) (:y pos) :has-dirt?] false)))

      ;; else
      (let [[in-boundary? x-move y-move]
            (cond 
              (= :left move)
              [(comp pos? :x) ;; x must already be positive
               dec            ;; x moves left
               identity]      ;; y does not move

              (= :right move)
              [(fn [{:keys [x]}]
                 (< (inc x) (:x dimensions)))   ;; must be room to the right
               inc            ;; x moves right
               identity]      ;; y does not move

              (= :down move)
              [(comp pos? :y)  ;; y must already be positive
               identity        ;; x does not move
               dec]            ;; y decrements

              (= :up move)
              [(fn [{:keys [y]}]
                 (< (inc y) (:y dimensions)))  ;; must be room upwards
               identity       ;; x does not move
               inc]           ;; y moves up


              :else (assert nil (str "Bad move: " move)))]
        (when (in-boundary? pos)
          (let [old-pos pos
                new-pos (-> old-pos
                            (update-in [:x] x-move)
                            (update-in [:y] y-move))]
            (-> g
                update-common
                (assoc :pos new-pos)
                (assoc-in [:state (:x old-pos) (:y old-pos) :has-vacuum?] false)
                (assoc-in [:state (:x new-pos) (:y new-pos) :has-vacuum?] true))))))))

;  (Set Move)
(def moves #{:left :right :up :down :suck})

;  (defalias Explored '[State

;  (Set 
(defn already-explored? [e node]
  (contains? e [(:state node) (:move node)]))

(defn remember-explored [e node]
  (conj e [(:state node) (:move node)]))

; Priority map implementation

; [Frontier (Seqable Node) -> Frontier]
(defn pmap-extend-frontier [frontier states]
  (letfn [(calc-priority [s]
            {:pre [(map? s)]}
            (+ (:cost s)
               (apply + (map (fn [row]
                               (apply + (map (fn [{:keys [has-dirt?] :as m}]
                                               (assert (contains? m :has-dirt?)
                                                       (str m row))
                                               (if has-dirt? 1 0))
                                             row)))
                             (:state s)))))]
    (into frontier (map (fn [s]
                          [s (calc-priority s)])
                        states))))

; [Frontier -> Node]
(defn pmap-peek-frontier [frontier]
  {:post [(map? %)]}
  (first (peek frontier)))

; [Frontier -> Frontier]
(def pmap-pop-frontier pop)

; [-> Frontier]
(defn pmap-empty-frontier []
  (priority-map))

;(defalias Config
;  '{:extend-frontier [Frontier (Seqable Node) -> Frontier]
;    :peek-frontier   [Frontier -> Node]
;    :pop-frontier    [Frontier -> Frontier]
;    :empty-frontier  [-> Frontier]})

; [-> Config]
(defn priority-map-frontier-config []
  {:extend-frontier pmap-extend-frontier
   :peek-frontier   pmap-peek-frontier
   :pop-frontier    pmap-pop-frontier
   :empty-frontier  pmap-empty-frontier})

; [-> Config]
(defn vector-frontier-config []
  {:extend-frontier into
   :peek-frontier   first
   :pop-frontier    (comp vec next)
   :empty-frontier  vector})

;; (Set '[Map Move])
(defn empty-explored [] #{})

;  [Config Node -> Node]
(defn search-common [{:keys [extend-frontier 
                             peek-frontier
                             pop-frontier
                             empty-frontier]
                      :as config}
                     init]
  {:pre [(map? init)]}
  (loop [frontier (extend-frontier (empty-frontier) [init])
         explored (empty-explored)]
    (if (empty? frontier)
      nil
      (let [node     (peek-frontier frontier)
            frontier (pop-frontier  frontier)]
        (if (goal-state? node)
          node
          (let [explored (remember-explored explored node)
                ;; keep only legal moves we have not already explored
                new-states (keep (fn [move]
                                   (let [n (apply-move node move)]
                                     (when (and n
                                                (not (already-explored? explored n)))
                                       n)))
                                 moves)
                _ (swap! (:stats node) update :max-branching max (count new-states))
                frontier (extend-frontier
                           frontier
                           new-states)]
            (swap! (:stats node) update :search-cost inc)
            (recur frontier explored)))))))

(defn determine-cost [config init]
  (:cost (search-common config init)))

(defn determine-path* [r]
  (loop [path '()
         r r]
    (when r
      (if-let [move (:move r)]
        (recur (conj path move)
               (:parent r))
        path))))

(defn determine-path [config init]
  (determine-path* (search-common config init)))

(defn search-stats [config init]
  (let [r (search-common config init)]
    (merge @(:stats r)
           (select-keys r [:cost])
           {:path (determine-path* r)})))

;(search-bfs (init-map))
(determine-cost (priority-map-frontier-config) (init-map))
(determine-path (priority-map-frontier-config) (init-map))

(determine-cost (vector-frontier-config) (init-map))
(determine-path (vector-frontier-config) (init-map))

(search-stats (priority-map-frontier-config) (init-map))
(search-stats (vector-frontier-config) (init-map))

#_(search-stats (vector-frontier-config) (init-map 3 3
                                                 (fn [_ _]
                                                   (< 0.2 (rand)))))

;; Reflex agent that sucks if dirt
(search-stats (vector-frontier-config) (init-map 3 3
                                                 (fn [_ _]
                                                   (< 0.2 (rand)))))
;{:search-cost 43922, :max-branching 5, :cost 14, 
; :path (:suck :up :suck :left :suck :down :down :suck :right :suck :right :suck :up :suck)}

;; A* search agent
(search-stats (priority-map-frontier-config) (init-map 3 3
                                                 (fn [_ _]
                                                   (< 0.2 (rand)))))
;{:search-cost 4019, :max-branching 5, :cost 14, 
; :path (:suck :down :suck :right :suck :up :suck :left :up :suck :left :suck :down :suck)}

#_
(dotimes [_ 5]
  (time
    (prn 
      (search-stats (priority-map-frontier-config) (init-map 3 3
                                                             (fn [_ _]
                                                               (< 0.2 (rand))))))))
;{:search-cost 4533, :max-branching 5, :cost 14, :path (:suck :up :suck :right :suck :down :suck :down :suck :left :left :suck :up :suck)}
;"Elapsed time: 612.120436 msecs"
;{:search-cost 8524, :max-branching 5, :cost 16, :path (:suck :down :suck :right :suck :up :up :suck :left :suck :left :suck :down :suck :down :suck)}
;"Elapsed time: 1390.728742 msecs"
;{:search-cost 5018, :max-branching 5, :cost 14, :path (:suck :down :suck :right :suck :up :suck :up :suck :left :left :suck :down :suck)}
;"Elapsed time: 720.46617 msecs"
;{:search-cost 15451, :max-branching 5, :cost 17, :path (:suck :left :suck :up :suck :right :suck :right :suck :down :suck :down :suck :left :suck :left :suck)}
;"Elapsed time: 2560.01815 msecs"
;{:search-cost 12240, :max-branching 5, :cost 16, :path (:suck :right :suck :up :suck :left :suck :left :suck :down :down :suck :right :suck :right :suck)}
;"Elapsed time: 1863.027061 msecs"

;(search-stats (vector-frontier-config) (init-map 20 20))

(do
  ;; reflex
  (time
    (prn
    (search-stats (vector-frontier-config) (init-map 3 3))))
  ;; A*
  (time
    (prn
      (search-stats (priority-map-frontier-config) (init-map 3 3)))))
;{:search-cost 220, :max-branching 4, :cost 7, :path (:up :right :suck :left :suck :left :suck)}
;"Elapsed time: 15.037175 msecs"
;{:search-cost 78, :max-branching 4, :cost 7, :path (:up :right :suck :left :suck :left :suck)}
;"Elapsed time: 14.849776 msecs"

(do
  ;; reflex
  (time
    (prn
    (search-stats (vector-frontier-config) (init-map 5 5))))
  ;; A*
  (time
    (prn
      (search-stats (priority-map-frontier-config) (init-map 5 5)))))
;{:search-cost 17238, :max-branching 4, :cost 13, 
; :path (:up :up :right :right :suck :left :suck :left :suck :left :suck :left :suck)}
;"Elapsed time: 5711.926171 msecs"
;{:search-cost 1588, :max-branching 4, :cost 13, 
; :path (:up :up :suck :left :suck :left :suck :right :right :right :suck :right :suck)}
;"Elapsed time: 208.511224 msecs"

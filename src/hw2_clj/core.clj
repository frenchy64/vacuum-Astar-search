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
;     :dimensions '{:x Int :y Int}})

; [-> Node]
(defn init-map []
  (let [s (vec
            (for [i (range 3)]
              (vec
                (for [j (range 3)]
                  {:x i
                   :y j
                   :has-dirt? (= 2 j)
                   :has-vacuum? (= 1 i j)}))))]
    {:state s
     :parent nil
     :pos {:x 1 :y 1}
     :move nil
     :cost 0
     :dimensions {:x 3 :y 3}}))

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
  "Returns nil if this is an illegal move, otherwise applies the move."
  [{:keys [pos state dimensions] :as g} move]
  (letfn [(update-common [m]
            (-> m
                (assoc :parent g)
                (update :cost inc)
                (assoc :move move)))]
    (case move
      (:suck) (-> g
                  update-common
                  (assoc-in [:state (:x pos) (:y pos) :has-dirt?] false))

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
            (apply + (map (fn [row]
                            (apply + (map (fn [{:keys [has-dirt?] :as m}]
                                            (assert (contains? m :has-dirt?)
                                                    (str m row))
                                            (if has-dirt? 1 0))
                                          row)))
                          (:state s))))]
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
                frontier (extend-frontier
                           frontier
                           (keep (fn [move]
                                   (let [n (apply-move node move)]
                                     (when (and n
                                                (not (already-explored? explored n)))
                                       n)))
                                 moves))]
            (recur frontier explored)))))))

(defn determine-cost [config init]
  (:cost (search-common config init)))

(defn determine-path [config init]
  (loop [path '()
         r (search-common config init)]
    (when r
      (if-let [move (:move r)]
        (recur (conj path move)
               (:parent r))
        path))))

;(search-bfs (init-map))
(determine-cost (priority-map-frontier-config) (init-map))
(determine-path (priority-map-frontier-config) (init-map))

(determine-cost (vector-frontier-config) (init-map))
(determine-path (vector-frontier-config) (init-map))

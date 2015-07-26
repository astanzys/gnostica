(ns gnostica.core
  (:import (clojure.lang ExceptionInfo)))

; game structure example
{:board {
         {:x 0 :y 0} {:minions {:id 0 :direction :up}}
         {:x 0 :y 1} {:minions {:id 1 :direction :east}}}}

(def direction-vectors
  {:north {:x 0 :y 1}
   :east  {:x 1 :y 0}
   :west  {:x -1 :y 0}
   :south {:x 0 :y -1}
   :up    {:x 0 :y 0}})

(defn- create-board [size]
  (let [coords (for [x (range size)
                     y (range size)]
                 {:x x :y y})]
    (zipmap coords (repeat {:minions {}}))))

(defn- calculate-target-loc [loc minion]
  (let [{minion-direction :direction} minion
        minion-vector (direction-vectors minion-direction)
        target-loc {:x (+ (loc :x) (minion-vector :x)) :y (+ (loc :y) (minion-vector :y))}]
    target-loc))

(defn create-game [board-size]
  {:board (create-board board-size)})

(defn move [game from target-id]
  (let [minion (get-in game [:board from :minions target-id])
        to (calculate-target-loc from minion)
        verify-validity
        (fn [game from to minion]
          (cond
            (= (minion :direction) :up)
            (throw (ex-info "movement when minion is facing up is illegal" {:from from :target target-id}))
            (not (contains? (game :board) to))
            (throw (ex-info (str to " is not on board") {:target target-id :from from :to to}))))]
    (verify-validity game from to minion)
    (let [copy-to-new-loc (assoc-in game [:board to :minions (minion :id)] minion)
          remove-from-old-loc (update-in copy-to-new-loc [:board from :minions] dissoc (minion :id))]
      remove-from-old-loc)))




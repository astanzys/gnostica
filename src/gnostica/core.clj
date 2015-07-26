(ns gnostica.core)

(def unit-vectors
  {:north {:x 0 :y 1}
   :east {:x 1 :y 0}
   :west {:x -1 :y 0}
   :south {:x 0 :y -1}
   :up {:x 0 :y 0}})

(defn- create-board [size]
  (let [coords (for [x (range size)
                     y (range size)]
                 {:x x :y y})]
    (zipmap coords (repeat {}))))

(defn calculate-target-loc [loc minion]
  (let [{minion-orientation :orientation} minion
        minion-vector (unit-vectors minion-orientation)
        target-loc {:x (+ (loc :x) (minion-vector :x)) :y (+ (loc :y) (minion-vector :y))}]
    target-loc))

(defn create-game [board-size]
  {:board (create-board board-size)})



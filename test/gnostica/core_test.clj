(ns gnostica.core-test
  (:require [clojure.test :refer :all]
            [gnostica.core :refer :all]))

(defn- place-minion [game loc minion]
  (assoc-in game [:board loc :minions (minion :id)] minion))

(defn- change-direction [game loc new-direction]
  (assoc-in game [:board loc :minions "test-minion" :direction] new-direction))

(defn- test-move [game from to]
  (let [move-result (move game from "test-minion")]
    (is (= (get-in move-result [:board from :minions "test-minion" :id]) nil))
    (is (= (get-in move-result [:board to :minions "test-minion" :id]) "test-minion"))
    move-result))

(deftest movement-test
  (testing "moving around in a square"
    (let [empty-board (create-game 3)
          minion {:id "test-minion" :direction :north}
          game (place-minion empty-board {:x 1 :y 1} minion)]
      (-> game
          ; go up
          (test-move {:x 1 :y 1} {:x 1 :y 2})
          (change-direction {:x 1 :y 2} :east)
          ; then east
          (test-move {:x 1 :y 2} {:x 2 :y 2})
          (change-direction {:x 2 :y 2} :south)
          ; then down
          (test-move {:x 2 :y 2} {:x 2 :y 1})
          (change-direction {:x 2 :y 1} :west)
          ; then right, should end up at starting square
          (test-move {:x 2 :y 1} {:x 1 :y 1})))))

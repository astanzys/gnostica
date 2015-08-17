(ns gnostica.core-test
  (:require [clojure.test :refer :all]
            [gnostica.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defn- place-minion [game loc minion]
  (assoc-in game [:board loc :minions (minion :id)] minion))

(defn- change-direction [game loc new-direction]
  (assoc-in game [:board loc :minions "test-minion" :direction] new-direction))

(defn- test-move [game from to]
  (let [move-result (move game from "test-minion")]
    (is (= (get-in move-result [:board from :minions "test-minion" :id]) nil))
    (is (= (get-in move-result [:board to :minions "test-minion" :id]) "test-minion"))
    move-result))

(defn- minion-not-present? [game loc id]
  (nil? (get-in game [:board loc id])))

(deftest movement-test
  (testing "moving around in a square"
    (let [empty-board (create-game 3)
          minion {:id "test-minion" :direction :north}
          game (place-minion empty-board {:x 1 :y 1} minion)]
      (-> game
          ; go north
          (test-move {:x 1 :y 1} {:x 1 :y 2})
          ; then east
          (change-direction {:x 1 :y 2} :east)
          (test-move {:x 1 :y 2} {:x 2 :y 2})
          ; then south
          (change-direction {:x 2 :y 2} :south)
          (test-move {:x 2 :y 2} {:x 2 :y 1})
          ; then west, should end up at starting square
          (change-direction {:x 2 :y 1} :west)
          (test-move {:x 2 :y 1} {:x 1 :y 1}))))

  (testing "moving is illegal when minion is facing upwards"
    (let [empty-board (create-game 3)
          minion {:id "test-minion" :direction :up}
          game (place-minion empty-board {:x 1 :y 1} minion)]
      (is (thrown? ExceptionInfo (move game {:x 1 :y 1} "test-minion")))))

  (testing "moving outside the board is illegal"
    (let [empty-board (create-game 3)
          game (-> empty-board
                   (place-minion {:x 0 :y 1} {:id "west" :direction :west}))]
      (is (thrown? ExceptionInfo (move game {:x 0 :y 1} "west"))))))

(deftest shrink-test
  (testing "shrinking stuff"
    (let [empty-board (create-game 3)
          first-minion {:id "first-minion" :direction :east}
          second-minion {:id "second-minion" :direction :west}
          game (-> empty-board
                   (place-minion {:x 0 :y 0} first-minion)
                   (place-minion {:x 1 :y 0} second-minion))]
      (is (minion-not-present?
            (shrink game {:x 0 :y 0} "first-minion" {:x 1 :y 0} "second-minion")
            {:x 1 :y 0} "second-minion")))))

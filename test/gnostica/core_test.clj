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
    (let [empty-board (create-game 3 ["p1"])
          minion {:id "test-minion" :direction :north :owner "p1"}
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
    (let [empty-board (create-game 3 ["p1"])
          minion {:id "test-minion" :direction :up :owner "p1"}
          game (place-minion empty-board {:x 1 :y 1} minion)]
      (is (thrown? ExceptionInfo (move game {:x 1 :y 1} "test-minion")))))

  (testing "moving outside the board is illegal"
    (let [empty-board (create-game 3 ["p1"])
          game (-> empty-board
                   (place-minion {:x 0 :y 1} {:id "west" :direction :west :owner "p1"}))]
      (is (thrown? ExceptionInfo (move game {:x 0 :y 1} "west")))))
  (testing "moving enemy minions is illegal"
    (let [empty-board (create-game 3 ["p1", "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 1} {:id "enemy-minion" :direction :east :owner "p2"}))]
      (is (thrown? ExceptionInfo (move game {:x 0 :y 1} "enemy-minion")))))
  (testing "after move player is changed"
    (let [empty-board (create-game 3 ["p1" "p2"])
          first-minion {:id "p1-minion" :direction :east :owner "p1"}
          second-minion {:id "p2-minion" :direction :west :owner "p2"}
          game (-> empty-board
                   (place-minion {:x 0 :y 0} first-minion)
                   (place-minion {:x 1 :y 0} second-minion))
          after-first-move (move game {:x 0 :y 0} "p1-minion")
          after-second-move (move after-first-move {:x 1 :y 0} "p2-minion")]
      (is (= (:current-player after-first-move) "p2"))
      (is (= (:current-player after-second-move) "p1")))))

(deftest shrink-test
  (testing "shrinking stuff"
    (let [empty-board (create-game 3 ["p1"])
          first-minion {:id "first-minion" :direction :east :owner "p1"}
          second-minion {:id "second-minion" :direction :west :owner "p1"}
          game (-> empty-board
                   (place-minion {:x 0 :y 0} first-minion)
                   (place-minion {:x 1 :y 0} second-minion))]
      (is (minion-not-present?
            (shrink game {:x 0 :y 0} "first-minion" {:x 1 :y 0} "second-minion")
            {:x 1 :y 0} "second-minion"))))
  (testing "using shrink with enemy minion as a source is illegal"
    (let [empty-board (create-game 3 ["p1", "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "own-minion" :direction :east :owner "p1"})
                   (place-minion {:x 1 :y 0} {:id "enemy-minion" :direction :west :owner "p2"})
                   )]
      (is (thrown? ExceptionInfo (shrink game {:x 1 :y 0} "enemy-minion" {:x 0 :y 0} "own-minion")))))
  (testing "after shrink player is changed"
    (let [empty-board (create-game 3 ["p1" "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "p11-minion" :direction :east :owner "p1"})
                   (place-minion {:x 1 :y 0} {:id "p21-minion" :direction :east :owner "p2"})
                   (place-minion {:x 0 :y 1} {:id "p22-minion" :direction :west :owner "p2"})
                   (place-minion {:x 1 :y 1} {:id "p12-minion" :direction :west :owner "p1"})
                   )
          after-first-move (shrink game {:x 0 :y 0} "p11-minion" {:x 1 :y 0} "p21-minion")
          after-second-move (shrink after-first-move {:x 0 :y 1} "p22-minion" {:x 1 :y 1} "p12-minion")]
      (is (= (:current-player after-first-move) "p2"))
      (is (= (:current-player after-second-move) "p1")))))

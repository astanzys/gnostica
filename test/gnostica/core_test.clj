(ns gnostica.core-test
  (:require [clojure.test :refer :all]
            [gnostica.core :refer :all])
  (:import (clojure.lang ExceptionInfo)))

(defn- place-minion
  ([game loc minion]
   (assoc-in game [:board loc :minions (minion :id)] minion))
  ([game minion-data]
   (let [[loc minion] minion-data]
     (assoc-in game [:board loc :minions (minion :id)] minion))))

(defn- change-direction [game loc new-direction]
  (assoc-in game [:board loc :minions "test-minion" :direction] new-direction))

(defn- test-move [game from to]
  (let [move-result (move game from "test-minion")]
    (is (= (get-in move-result [:board from :minions "test-minion" :id]) nil))
    (is (= (get-in move-result [:board to :minions "test-minion" :id]) "test-minion"))
    move-result))

(defn- minion-not-present? [game loc id]
  (nil? (get-in game [:board loc id])))


(defn- setup-board [players minions]
  (let [empty-board (create-game 3 players)
        populated (reduce place-minion empty-board minions)]
    populated))

(deftest movement-test
  (testing "moving around in a square"
    (let [empty-board (create-game 3 ["p1"])
          minion {:id "test-minion" :direction :north :owner "p1"  :size 1}
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
          minion {:id "test-minion" :direction :up :owner "p1"  :size 1}
          game (place-minion empty-board {:x 1 :y 1} minion)]
      (is (thrown? ExceptionInfo (move game {:x 1 :y 1} "test-minion")))))

  (testing "moving outside the board is illegal"
    (let [empty-board (create-game 3 ["p1"])
          game (-> empty-board
                   (place-minion {:x 0 :y 1} {:id "west" :direction :west :owner "p1" :size 1}))]
      (is (thrown? ExceptionInfo (move game {:x 0 :y 1} "west")))))
  (testing "moving enemy minions is illegal"
    (let [empty-board (create-game 3 ["p1", "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 1} {:id "enemy-minion" :direction :east :owner "p2" :size 1}))]
      (is (thrown? ExceptionInfo (move game {:x 0 :y 1} "enemy-minion")))))
  (testing "after move player is changed"
    (let [empty-board (create-game 3 ["p1" "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "p1-minion" :direction :east :owner "p1" :size 1})
                   (place-minion {:x 1 :y 0} {:id "p2-minion" :direction :west :owner "p2" :size 1}))
          after-first-move (move game {:x 0 :y 0} "p1-minion")
          after-second-move (move after-first-move {:x 1 :y 0} "p2-minion")]
      (is (= (:current-player after-first-move) "p2"))
      (is (= (:current-player after-second-move) "p1"))))
  (testing "move respects 3 minions per square rule"
    (let [game (setup-board ["p1"]
                            [[{:x 0 :y 0} {:id "1" :direction :up :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "2" :direction :up :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "3" :direction :up :owner "p1" :size 1}]
                             [{:x 1 :y 0} {:id "4" :direction :west :owner "p1" :size 1}]])]
      (is (thrown? ExceptionInfo (move game {:x 1 :y 0} "4"))))))

(deftest shrink-test
  (testing "shrinking stuff"
    (let [game (setup-board ["p1"]
                            [
                             [{:x 0 :y 0} {:id "size-1" :direction :east :owner "p1" :size 1}]
                             [{:x 1 :y 0} {:id "target-1" :direction :west :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "size-2" :direction :east :owner "p1" :size 2}]
                             [{:x 1 :y 0} {:id "target-2" :direction :west :owner "p1" :size 3}]
                             ])]
      (is (minion-not-present?
            (shrink game {:x 0 :y 0} "size-1" {:x 1 :y 0} "target-1" 1)
            {:x 1 :y 0} "target-1"))
      (is (=
            1
            (get-in
              (shrink game {:x 0 :y 0} "size-2" {:x 1 :y 0} "target-2" 2)
              [:board {:x 1 :y 0} :minions "target-2" :size])))))
  (testing "using shrink with enemy minion as a source is illegal"
    (let [empty-board (create-game 3 ["p1", "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "own-minion" :direction :east :owner "p1" :size 1})
                   (place-minion {:x 1 :y 0} {:id "enemy-minion" :direction :west :owner "p2" :size 1})
                   )]
      (is (thrown? ExceptionInfo (shrink game {:x 1 :y 0} "enemy-minion" {:x 0 :y 0} "own-minion" 1)))))
  (testing "after shrink player is changed"
    (let [empty-board (create-game 3 ["p1" "p2"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "p11-minion" :direction :east :owner "p1" :size 1})
                   (place-minion {:x 1 :y 0} {:id "p21-minion" :direction :east :owner "p2" :size 1})
                   (place-minion {:x 0 :y 1} {:id "p22-minion" :direction :east :owner "p2" :size 1})
                   (place-minion {:x 1 :y 1} {:id "p12-minion" :direction :west :owner "p1" :size 1}))
          after-first-move (shrink game {:x 0 :y 0} "p11-minion" {:x 1 :y 0} "p21-minion" 1)
          after-second-move (shrink after-first-move {:x 0 :y 1} "p22-minion" {:x 1 :y 1} "p12-minion" 1)]
      (is (= (:current-player after-first-move) "p2"))
      (is (= (:current-player after-second-move) "p1"))))
  (testing "shrinking is not possible if afterwards player would have more than 5 minions of same size"))

(deftest create-test
  (testing "create minion"
    (let [empty-board (create-game 3 ["p1"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "some-dude" :direction :up :owner "p1" :size 1}))
          [after-create new-minion-id] (create game {:x 0 :y 0} "some-dude" :up)
          minions-at-0-0 (get-in after-create [:board {:x 0 :y 0} :minions])
          new-minion (minions-at-0-0 new-minion-id)]
      (is (= (count minions-at-0-0) 2))
      (is (= (:owner new-minion) "p1"))
      (is (= (:size new-minion) 1))))
  (testing "cannot create using other player minions as a source"
    (let [empty-board (create-game 3 ["p1"])
          game (-> empty-board
                   (place-minion {:x 0 :y 0} {:id "some-dude" :direction :up :owner "p5" :size 1}))]
      (is (thrown? ExceptionInfo (create game {:x 0 :y 0} "some-dude" :up)))))
  (testing "3 minions per tile rule"
    (let [game (setup-board ["p1"]
                            [[{:x 0 :y 0} {:id "1" :direction :up :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "2" :direction :up :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "3" :direction :up :owner "p1" :size 1}]])]
      (is (thrown? ExceptionInfo (create game {:x 0 :y 0} "1" :up)))))
  (testing "5 minions of same size per player rule"
    (let [game (setup-board ["p1"]
                            [[{:x 0 :y 0} {:id "1" :direction :up :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "2" :direction :up :owner "p1" :size 1}]
                             [{:x 0 :y 0} {:id "3" :direction :up :owner "p1" :size 1}]
                             [{:x 1 :y 0} {:id "4" :direction :up :owner "p1" :size 1}]
                             [{:x 1 :y 0} {:id "5" :direction :up :owner "p1" :size 1}]])]
      (is (thrown? ExceptionInfo (create game {:x 1 :y 0} "5" :up))))))

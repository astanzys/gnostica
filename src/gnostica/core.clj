(ns gnostica.core)

; TODO move other pieces,
; TODO shrink pieces, shrink tiles,
; TODO grow pieces, grow land,
; TODO create pieces, create tiles
; TODO reorientation as a move, reorientation after using a minion
; TODO players, move cycle
; TODO deck, cards, board layout, board changes
; TODO victory conditions and handling
; TODO player elimination rules
; TODO special cards

; game data example
{:board
 {{:y 0, :x 0}
               {:minions {"some-dude" {:id "some-dude", :direction :up}}},
  {:y 1, :x 0} {:minions {}}}}

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

(defn move [game from source-id]
  (let [minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from minion)
        verify-validity
        (fn [game from to minion]
          (cond
            (= (minion :direction) :up)
            (throw (ex-info "movement when minion is facing up is illegal" {:from from :target source-id}))
            (not (contains? (game :board) to))
            (throw (ex-info (str to " is not on board") {:target source-id :from from :to to}))))]
    (verify-validity game from to minion)
    (let [copy-to-new-loc (assoc-in game [:board to :minions (minion :id)] minion)
          remove-from-old-loc (update-in copy-to-new-loc [:board from :minions] dissoc (minion :id))]
      remove-from-old-loc)))

(defn shrink [game from source-id to target-id]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion)
        target-minion (get-in game [:board to :minions target-id])]
    (update-in game [:board to :minions] dissoc target-id)))






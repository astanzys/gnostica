(ns gnostica.core)

; TODO grow pieces, grow land,
; TODO create pieces, create tiles
; TODO move other pieces,
; TODO shrink tiles,
; TODO reorientation as a move, reorientation after using a minion
; TODO minion sizes
; TODO players, move cycle
; TODO deck, cards, board layout, board changes
; TODO victory conditions and handling
; TODO player elimination rules
; TODO special cards

; game data example
{:players ["p1", "p2", "p3"]
 :current-player "p2"
 :next-players ["p3" "p1" "p2"] ; repeats
 :board
 {{:y 0, :x 0}
               {:minions {"some-dude" {:id "some-dude", :direction :up :owner "p1"}}},
  {:y 1, :x 0}
               {:minions {}}}}

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

(defn create-game [board-size players]
  {:board (create-board board-size)
   :current-player (first players)
   :next-players (rest (cycle players)) ; rest to skip first player
   })

(defn end-turn [game]
  (-> game
      (assoc :current-player (first (:next-players game)))
      (assoc :next-players (rest (:next-players game)))))

(defn move [game from source-id]
  (let [minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from minion)
        verify-validity
        (fn [game from to minion]
          (cond
            (not= (:owner minion) (:current-player game))
            (throw (ex-info "moving enemy minions is illegal" {:source source-id}))
            (= (minion :direction) :up)
            (throw (ex-info "movement when minion is facing up is illegal" {:from from :target source-id}))
            (not (contains? (game :board) to))
            (throw (ex-info (str to " is not on board") {:target source-id :from from :to to}))))]
    (verify-validity game from to minion)
    (let [copy-to-new-loc (assoc-in game [:board to :minions (minion :id)] minion)
          remove-from-old-loc (update-in copy-to-new-loc [:board from :minions] dissoc (minion :id))]
      (end-turn remove-from-old-loc))))

(defn shrink [game from source-id to target-id]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion)
        target-minion (get-in game [:board to :minions target-id])]
    (update-in game [:board to :minions] dissoc target-id)))






(ns gnostica.core)

; TODO create pieces, create tiles
; TODO grow pieces, grow land,
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

(def id-generator (atom 0))                                 ;temp, its a bit silly because you have to increment manually

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

(defn- verify-is-using-own-minion [game minion]
  (when (not= (:owner minion) (:current-player game))
    (throw (ex-info "using enemy minion as a source is illegal" {:minion minion}))))

(defn move [game from source-id]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion)
        verify-validity
        (fn [game from to minion]
          (verify-is-using-own-minion game minion)
          (cond
            (= (minion :direction) :up)
            (throw (ex-info "movement when minion is facing up is illegal" {:from from :target source-id}))
            (not (contains? (game :board) to))
            (throw (ex-info (str to " is not on board") {:target source-id :from from :to to}))))]
    (verify-validity game from to source-minion)
    (let [copy-to-new-loc (assoc-in game [:board to :minions (source-minion :id)] source-minion)
          remove-from-old-loc (update-in copy-to-new-loc [:board from :minions] dissoc (source-minion :id))]
      (end-turn remove-from-old-loc))))

(defn shrink [game from source-id to target-id]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion)
        target-minion (get-in game [:board to :minions target-id])
        verify-validitiy
        (fn [game from source-id to target-id]
          (cond
            (not= (:owner source-minion) (:current-player game))
            (throw (ex-info "using enemy minion as a source is illegal" {:source source-id}))))]
    (verify-validitiy game from source-id to target-id)
    (end-turn
      (update-in game [:board to :minions] dissoc target-id))))

(defn create [game from source-id new-minion-orientation]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion)
        new-minion-id @id-generator
        verify-validity
        (fn [game]
          (verify-is-using-own-minion game source-minion)
          (when (> (count (get-in game [:board from :minions])) 2)
            (throw (ex-info "max 3 minions per square" {:source source-id}))))
        create
        (update-in game
                   [:board to :minions]
                   assoc new-minion-id {
                                        :id new-minion-id
                                        :orientation new-minion-orientation
                                        :owner (:owner source-minion)})]
    (verify-validity game)
    (swap! id-generator inc)
    [(end-turn create) new-minion-id]))






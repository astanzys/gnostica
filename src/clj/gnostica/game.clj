(ns gnostica.game
  (:use [clojure.algo.generic.functor :only (fmap)]))

; TODO grow land,
; TODO create tiles
; TODO move other pieces, move more than one square
; TODO shrink tiles,
; TODO reorientation as a move, reorientation after using a minion
; TODO minion sizes - grow
; TODO initial board setup
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
               {:minions {"some-dude" {:id "some-dude", :direction :up :owner "p1" :size 1}}},
  {:y 1, :x 0}
               {:minions {}}}}

(def direction-vectors
  {:north {:x 0 :y 1}
   :east  {:x 1 :y 0}
   :west  {:x -1 :y 0}
   :south {:x 0 :y -1}
   :up    {:x 0 :y 0}})

(def id-generator (atom 0))

(defn- get-next-minion-id []
  (let [ret-val @id-generator]
    (swap! id-generator inc)
    ret-val))

(defn- find-minions-per-player [game]
  (->> game
       :board
       vals
       (map :minions)
       (apply merge)
       vals
       (group-by :owner)))

(defn- create-board [size]
  (let [coords (for [x (range size)
                     y (range size)]
                 {:x x :y y})]
    (zipmap coords (repeat {:minions {}}))))

(defn- calculate-target-loc [loc minion distance]
  (let [{minion-direction :direction} minion
        minion-vector (fmap (partial * distance) (minion-direction direction-vectors))
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

(defn- verify-minion-power-limit [game minion target-amount]
  (when (> target-amount (:size minion))
    (throw (ex-info "cannot move or shrink by more than the size of source"
                    {:game game :source minion :target-amount target-amount}))))

(defn- verify-adding-minion-to-loc [game to]
  (when (> (count (get-in game [:board to :minions])) 2)
    (throw (ex-info "max 3 minions per square" {:game game :to to}))))

(defn- verify-adding-new-minion [game player size]
  (let [minions-per-player (find-minions-per-player game)
        players-minions (get minions-per-player player)
        minions-of-size (filter #(= size (:size %)) players-minions)]
    (when (> (count minions-of-size) 4)
      (throw (ex-info "cannot create additional minion" {:game game})))))

(defn- verify-loc-is-on-board [game loc]
  (when (not (contains? (game :board) loc))
    (throw (ex-info (str loc " is not on board") {:game game :loc loc}))))

(defn move [game from source-id distance]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion distance)
        verify-validity
        (fn [game from to minion]
          (verify-is-using-own-minion game minion)
          (verify-loc-is-on-board game to)
          (verify-adding-minion-to-loc game to)
          (verify-minion-power-limit game source-minion distance)
          (cond
            (= (minion :direction) :up)
            (throw (ex-info "movement when minion is facing up is illegal" {:from from :target source-id})))
          )]
    (verify-validity game from to source-minion)
    (let [copy-to-new-loc (assoc-in game [:board to :minions (source-minion :id)] source-minion)
          remove-from-old-loc (update-in copy-to-new-loc [:board from :minions] dissoc (source-minion :id))]
      (end-turn remove-from-old-loc))))

(defn shrink [game from source-id to target-id by]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion 1)
        target-minion (get-in game [:board to :minions target-id])
        new-target-minion-size (- (:size target-minion) by)
        verify-validitiy
        (fn []
          (verify-is-using-own-minion game source-minion)
          (verify-minion-power-limit game source-minion by)
          (when (pos? new-target-minion-size)
            (verify-adding-new-minion game (:owner target-minion) new-target-minion-size)))

        shrink-minion
        (fn []
          (if (pos? new-target-minion-size)
            (assoc-in game [:board to :minions target-id :size] new-target-minion-size)
            (update-in game [:board to :minions] dissoc target-id)))]
    (verify-validitiy)
    (end-turn (shrink-minion))))

(defn create [game from source-id new-minion-orientation]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion 1)
        new-minion-id (get-next-minion-id)
        verify-validity
        (fn [game]
          (verify-is-using-own-minion game source-minion)
          (verify-adding-minion-to-loc game to)
          (verify-adding-new-minion game (:current-player game) 1))
        create
        (update-in game
                   [:board to :minions]
                   assoc new-minion-id {
                                        :id new-minion-id
                                        :orientation new-minion-orientation
                                        :owner (:owner source-minion)
                                        :size 1})]
    (verify-validity game)
    [(end-turn create) new-minion-id]))

(defn grow [game from source-id target-id]
  (let [source-minion (get-in game [:board from :minions source-id])
        to (calculate-target-loc from source-minion 1)
        target-minion (get-in game [:board to :minions target-id])
        new-target-minion-size (inc (:size target-minion))

        verify-validity
        (fn []
          (verify-is-using-own-minion game source-minion)
          (verify-adding-new-minion game (:owner target-minion) new-target-minion-size)
          (when (> new-target-minion-size 3)
            (throw (ex-info "cannot grow past size 3" {:game game :source source-minion :target target-minion}))))

        grow
        (fn []
          (assoc-in game [:board to :minions target-id :size] new-target-minion-size))]
    (verify-validity)
    (end-turn (grow))))

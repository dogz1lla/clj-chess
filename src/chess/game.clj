(ns chess.game
  (:require [clojure.set :as s]
            [clojure.core.async :as async]
            [quil.core :as q]
            [chess.state :as state]
            [chess.moves :as moves]
            [chess.attacks :as attacks]))


(defn start-game []
  (state/init-state))

(defn make-move [from to state]
  (moves/move! from to state))

(defn make-attack [target-square attacker state]
  (attacks/attack! target-square attacker state))

(defn calculate-all-moves [{:keys [board] :as state}]
  (let [pieces (filter second board)
        update-fn (fn [[pos piece]] [pos (assoc piece :moves (moves/moves piece state))])
        updated-pieces (map update-fn pieces)]
    (assoc state :board (reduce (fn [m [k v]] (assoc m k v)) board updated-pieces))))

(defn calculate-all-attacks [{:keys [board] :as state}]
  (let [pieces (filter second board)
        update-fn (fn [[pos piece]] [pos (assoc piece :attacks (moves/moves piece state))])
        updated-pieces (map update-fn pieces)]
    (assoc state :board (reduce (fn [m [k v]] (assoc m k v)) board updated-pieces))))

(defn check? [{:keys [board turn] :as state}]
  (let [king (->> board
                  (filter second)
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (= turn color)))
                  first
                  second)]
    ; TODO next
    king))
                 

(defn mate? [])


(comment
  (let [piece-1 {:piece :king :color :white :pos [3, 3]}
        piece-2 {:piece :pawn :color :black :pos [5, 5]}
        piece-3 {:piece :pawn :color :black :pos [3, 8]}
        board (-> (state/init-board)
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2)
                  (state/put-piece-on-board piece-3))
        state {:board board :turn :white}]
    ; (calculate-all-moves state)
    ; (calculate-all-attacks state)
    (check? state)))

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
        update-fn (fn [[pos piece]] [pos (assoc piece :attacks (attacks/attacks piece state))])
        updated-pieces (map update-fn pieces)]
    (assoc state :board (reduce (fn [m [k v]] (assoc m k v)) board updated-pieces))))

(defn update-check [{:keys [board turn] :as state}]
  (let [king (->> board
                  (filter second)
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (=  turn color)))
                  first
                  second)
        king-pos (:pos king)
        opponent-color (case turn :white :black :black :white)
        checkers (filter
                   (fn [{:keys [attacks]}] (and attacks (attacks king-pos)))
                   (->> board
                        (filter second)
                        (map second)
                        (filter (fn [{:keys [color]}] (= color opponent-color)))))
        checked-by (when (seq checkers) (:id (first checkers)))]
    (assert (#{0 1} (count checkers)) "More than 1 checker at a time detected")
    (assoc-in state [:board king-pos] (assoc king :checked-by checked-by))))
    
  
(defn check? [{:keys [board turn] :as state}]
  (let [king (->> board
                  (filter second)
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (= turn color)))
                  first
                  second)]
    (not (nil? (:checked-by king)))))
                 

(defn mate?
  "How is mate position defined? It means that a king
  a) has free squares in next to it and
  b) moving to any of those squares will put it in check state."
  [{:keys [board turn] :as state}]
  (let [king (->> board
                  (filter second)
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (= turn color)))
                  first
                  second)]
    (not (nil? (:checked-by king)))))


(comment
  (let [piece-1 {:piece :king :color :white :pos [3, 3] :id "king1"}
        piece-2 {:piece :pawn :color :black :pos [2, 2] :id "pawn1"}
        piece-3 {:piece :pawn :color :black :pos [3, 8] :id "pawn2"}
        piece-4 {:piece :king :color :black :pos [1, 8] :id "king2"}
        board (-> (state/init-board)
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2)
                  (state/put-piece-on-board piece-3)
                  (state/put-piece-on-board piece-4))
        state (->> {:board board :turn :white}
                   (calculate-all-moves)
                   (calculate-all-attacks)
                   (update-check))]
    ; (update-check state)
    (check? state)))

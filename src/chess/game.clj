(ns chess.game
  (:require [clojure.set :as s]
            [clojure.core.async :as async]
            [quil.core :as q]
            [chess.state :as state]
            [chess.moves :as moves]
            [chess.attacks :as attacks]))


(defn init-game []
  (state/init-state))

(defn make-move [from to state]
  (moves/move! from to state))

(defn make-attack [attacker-square target-square state]
  (attacks/attack! attacker-square target-square state))

(defn calculate-all-moves [{:keys [board] :as state}]
  (let [white (:white board)
        black (:black board)
        update-fn (fn [[uid piece]] [uid (assoc piece :moves (moves/moves piece state))])
        ; TODO: probably not the most clean way to do that, can 'update' be used?
        white (reduce (fn [m [k v]] (assoc m k v)) white (map update-fn white))
        black (reduce (fn [m [k v]] (assoc m k v)) black (map update-fn black))]
    (-> state
        (assoc-in [:board :white] white)
        (assoc-in [:board :black] black))))

(defn calculate-all-attacks [{:keys [board] :as state}]
  (let [white (:white board)
        black (:black board)
        update-fn (fn [[uid piece]] [uid (assoc piece :attacks (attacks/attacks piece state))])
        ; TODO: probably not the most clean way to do that, can 'update' be used?
        white (reduce (fn [m [k v]] (assoc m k v)) white (map update-fn white))
        black (reduce (fn [m [k v]] (assoc m k v)) black (map update-fn black))]
    (-> state
        (assoc-in [:board :white] white)
        (assoc-in [:board :black] black))))

(defn update-check [{:keys [board turn] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (into black white)
        king (->> all-pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (=  turn color)))
                  first
                  second)
        king-id (:id king)
        king-pos (:pos king)
        king-color (:color king)
        opponent-color (case turn :white :black :black :white)
        checkers (filter
                   (fn [{:keys [attacks]}] (and attacks (attacks king-pos)))
                   (->> all-pieces
                        (map second)
                        (filter (fn [{:keys [color]}] (= color opponent-color)))))
        checked-by (when (seq checkers) (:id (first checkers)))]
    (assoc-in state [:board king-color king-id] (assoc king :checked-by checked-by))))
    

(defn switch-turn [{:keys [turn] :as state}]
  (assoc state :turn (case turn :white :black :black :white)))
  

(defn refresh-state [state]
  (-> state
      (calculate-all-moves)
      (calculate-all-attacks)
      (update-check)))
  
  
(defn check? [{:keys [board turn] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (into black white)
        king (->> all-pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (=  turn color)))
                  first
                  second)]
    (not (nil? (:checked-by king)))))
                 

(defn mate?
  "How is mate position defined? It means that a king
  a. has free squares in next to it and
  b. moving to any of those squares will put it in check state.
  NOTE: make sure this is called after all moves and attacks are updated

  Logic
  - get all moves and union with all attacks
  - for each of the positions from that union create a new state where the king is in that position
  - call update-check on each of those hypothetical states
  - return (every? check? [state-1 state-2 ...])
  "
  [{:keys [board turn] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (into black white)
        king (->> all-pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (=  turn color)))
                  first
                  second)
        king-pos (:pos king)
        future-moves (s/union (:moves king) (:attacks king))
        future-states (map #(moves/move! king-pos % state) future-moves)
        king-checked? (fn [st] (-> st
                                   (refresh-state)
                                   (check?)))]
    (every? king-checked? future-states)))

(defn game-over? [state]
  (mate? state))  


(defn run-game!
  "Need some kind of state machine. And also a game loop that awaits inputs from the user.
  The state machine could be in one of the following states:
  - awaiting a move/attack, taking into account whose turn it is
    + this one should be able to communicate to the player that there are special moves available
    on this particular move (eg castling, en-passante);
    + on some moves the player doesnt have as much freedom as on the normal move, eg, in a check
    position the player is forced to deal with the check;
  - check
  - mate (game over)
  
  user sends the message, gets new state in response
  "
  []
  (let [c-in (async/chan)
        c-out (async/chan)]
    (async/go-loop [msg {:type :game-start}
                    state (init-game)]
                    
      (if (= (:type msg) :game-over)
        ; FIXME this branch shouldnt exist probably
        (do
          (println "Game over!")
          (async/>! c-out (refresh-state state)))
        (let [msg-type (:type msg)
              msg-body (:body msg)
              next-state (case msg-type
                           :game-start state
                           :move (let [[from to] msg-body]
                                   (make-move from to state))
                           :attack (let [[from to] msg-body]
                                     (make-attack from to state)))
              next-state (refresh-state next-state)]
          (if (game-over? next-state)
            (do
              (println "Game overrrrrr")
              (async/>! c-out next-state))  ; FIXME return something more meaningful
            (do
              (let [next-state (switch-turn next-state)]
                (async/>! c-out next-state)
                (recur (async/<! c-in) next-state)))))))
    [c-in c-out]))


(comment
  (let [piece-1 {:piece :king :color :white :pos [3, 3] :id "king1"}
        piece-2 {:piece :king :color :black :pos [8, 8] :id "king2"}
        board (-> {}
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2))
        state {:board board}]
    (calculate-all-moves state))

  (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
        piece-2 {:piece :pawn :color :black :pos [7, 7] :id "pawn1"}
        board (-> {}
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2))
        state {:board board}]
    (calculate-all-attacks state))

  (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
        piece-2 {:piece :pawn :color :black :pos [7, 7] :id "pawn1"}
        board (-> {}
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2))
        state (-> {:board board :turn :white}
                  (calculate-all-attacks))]
    (update-check state))

  (let [piece-1 {:piece :king :color :white :pos [3, 3] :id "king1"}
        piece-2 {:piece :pawn :color :black :pos [2, 2] :id "pawn1"}
        piece-3 {:piece :pawn :color :black :pos [3, 8] :id "pawn2"}
        piece-4 {:piece :king :color :black :pos [1, 8] :id "king2"}
        board (-> {}
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2)
                  (state/put-piece-on-board piece-3)
                  (state/put-piece-on-board piece-4))
        state (->> {:board board :turn :white}
                   (calculate-all-moves)
                   (calculate-all-attacks)
                   (update-check))]
    (check? state))

  (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
        piece-2 {:piece :pawn :color :black :pos [8, 7] :id "pawn1"}
        piece-3 {:piece :pawn :color :black :pos [7, 6] :id "pawn2"}
        piece-4 {:piece :rook :color :black :pos [1, 7] :id "rook1"}
        board (-> {}
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2)
                  (state/put-piece-on-board piece-3)
                  (state/put-piece-on-board piece-4))
        state (->> {:board board :turn :white}
                   (calculate-all-moves)
                   (calculate-all-attacks)
                   (update-check))]
    (mate? state)))

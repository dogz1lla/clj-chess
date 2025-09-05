(ns chess.game
  (:require [clojure.set :as s]
            [clojure.core.async :as async]
            [quil.core :as q]
            [chess.state :as state]
            [chess.moves :as moves]
            [chess.attacks :as attacks]))


(defn init-game []
  (state/init-state))
  ; ; pawn promotion
  ; (state/init-state [{:kind :pawn  :white [[5 2]] :black [[5 7]]}
  ;                    {:kind :king  :white [[1 1]] :black [[8 8]]}])
  ; ; en-passant
  ; (state/init-state [{:kind :pawn  :white [[5 4]] :black [[4 2]]}
  ;                    {:kind :king  :white [[1 1]] :black [[8 8]]}]))

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
        

(defn refresh-state [state]
  (-> state
      (calculate-all-moves)
      (calculate-all-attacks)
      (update-check)))
    

(defn remove-invalid-moves
  "Remove moves that would result in check for the king.
  NOTE: this function should run after calculate-all-moves and calculate-all-attacks"
  [{:keys [board turn] :as state} piece-id]
  (let [allied-pieces (turn board)
        piece (get allied-pieces piece-id)
        _ (assert piece (str "Piece with id " piece-id "not found (turn " turn ")"))
        piece-moves (:moves piece)
        piece-attacks (:attacks piece)
        piece-pos (:pos piece)
        states-after-moves   (map #(make-move   piece-pos % state) piece-moves)
        states-after-attacks (map #(make-attack piece-pos % state) piece-attacks)
        king-checked? (fn [st] (-> st
                                   (refresh-state)
                                   (check?)))
        valid-moves-states   (map #(not (king-checked? %)) states-after-moves)
        valid-attacks-states (map #(not (king-checked? %)) states-after-attacks)
        filter-fn (fn [[valid? move-or-attack]] (when valid? move-or-attack))
        valid-moves   (->> (map vector valid-moves-states piece-moves)
                           (filter filter-fn)
                           (map second)
                           (set))
        valid-attacks (->> (map vector valid-attacks-states piece-attacks)
                           (filter filter-fn)
                           (map second)
                           (set))
        updated-piece (assoc (assoc piece :moves valid-moves) :attacks valid-attacks)]
    (assoc-in state [:board turn piece-id] updated-piece)))
    

(defn filter-out-checked-moves [{:keys [board turn] :as state}]
  (let [pieces-to-take-turn (turn board)
        ids (keys pieces-to-take-turn)]
    (reduce remove-invalid-moves state ids)))


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
  [{:keys [turn] :as state}]
  (let [state (refresh-state state)
        allied-pieces (turn (:board state))
        king (->> allied-pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  first
                  second)
        get-moves-list (fn [[_ {:keys [pos moves attacks]}]] [pos (s/union moves attacks)])
        possible-futures (map get-moves-list allied-pieces)
        calculate-futures-for-piece (fn [[pos moves-attacks]]
                                      (map #(moves/move! pos % state) moves-attacks))
        future-states (map calculate-futures-for-piece possible-futures)  ; list of lists of states
        future-states (reduce concat [] future-states)  ; list of states
        king-checked? (fn [st] (-> st
                                   (refresh-state)
                                   (check?)))]
    ; NOTE: doing (boolean ...) just to return false instead of nil (for OCD reasons)
    ; (and (boolean (seq future-moves)) (every? king-checked? future-states))
    ; WARNING: this could lead to bugs: if only pawns left and they are all blocked might be empty
    ; list of moves
    (every? king-checked? future-states)))


(defn promote-pawn [{:keys [turn board] :as state} pawn-id promote-to]
  (let [pawn-color (case turn :white :black :black :white)]
    (assert (get (pawn-color board) pawn-id) (str "Pawn " pawn-id " not found in " pawn-color))
    (assoc-in state [:board pawn-color pawn-id :piece] promote-to)))


(defn game-over? [state]
  ; NOTE need to switch the turn because this check is supposed to happen after enemy's turn
  (mate? (switch-turn state)))  


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
                    
      (if (= (:type msg) :game-start)
          (let [next-state (-> state
                               refresh-state
                               filter-out-checked-moves)]
            (println "Game start!")
            (async/>! c-out next-state)
            (recur (async/<! c-in) next-state))
        (let [msg-type (:type msg)
              msg-body (:body msg)
              next-state (case msg-type
                           :game-start state
                           :move (let [[from to] msg-body]
                                   (make-move from to state))
                           :attack (let [[from to] msg-body]
                                     (make-attack from to state))
                           :promote-pawn (let [{:keys [pawn-id piece]} msg-body]
                                           (promote-pawn state pawn-id piece)))
              next-state (refresh-state next-state)]
          (if (game-over? next-state)
            (do
              (println "Checkmate!")
              (async/>! c-out next-state))  ; FIXME return something more meaningful
            (do
              (let [next-state (if (= msg-type :promote-pawn)
                                 next-state                 ; if pawn promotion -> no turn switch
                                 (switch-turn next-state))  ; else switch turn before returning
                    next-state (filter-out-checked-moves next-state)]
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
    (mate? state)

   (let [king {:piece :king :pos [1, 1] :color :white :id "king"}
         rook {:piece :rook :pos [8, 2] :color :black :id "rook"}
         board (-> {}
                   (state/put-piece-on-board king)
                   (state/put-piece-on-board rook))
         state (-> {:board board :turn :white}
                   (refresh-state)
                   (filter-out-checked-moves))]
     (get-in state [:board :white "king" :moves])))

  (mate? (init-game)))

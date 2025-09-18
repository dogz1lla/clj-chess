(ns chess.game
  (:require [clojure.set :as s]
            [clojure.core.async :as async]
            [quil.core :as q]
            [chess.state :as state]
            [chess.moves :as moves]
            [chess.attacks :as attacks]))


(defn init-game []
  ; (state/init-state)
  ; ; pawn promotion
  ; (state/init-state [{:kind :pawn  :white [[5 2]] :black [[5 7]]}
  ;                    {:kind :king  :white [[1 1]] :black [[8 8]]}])
  ; ; en-passant
  ; (state/init-state [{:kind :pawn  :white [[5 4]] :black [[4 2]]}
  ;                    {:kind :king  :white [[1 1]] :black [[8 8]]}]))
  ; ; castling
  ; (state/init-state [{:kind :rook  :white [[1 8] [8 8]] :black [[1 1] [8 1]]}
  ;                    {:kind :king  :white [[5 8]] :black [[5 1]]}])
  ; dfs
  (state/init-state [{:kind :king :white [[1 8]] :black [[1 1]]}
                     {:kind :pawn
                      :white [[1 5] [3 5] [5 5] [7 5]]
                      :black [[1 4] [3 4] [5 4] [7 4]]}]))
                     ; {:kind :king :white [[8 8]] :black [[1 1]]}
                     ; {:kind :pawn 
                     ;  :white [[1 5]
                     ;          [2 5]
                     ;          [3 5]
                     ;          [4 5]
                     ;          [5 5]
                     ;          [6 5]
                     ;          [7 5]
                     ;          [8 5]] 
                     ;  :black [[1 4]
                     ;          [2 4]
                     ;          [3 4]
                     ;          [4 4]
                     ;          [5 4]
                     ;          [6 4]
                     ;          [7 4]
                     ;          [8 4]]}]))
                                                    

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

(defn checkmate? [state]
  (and (check? state) (mate? state)))

(defn stalemate? [state]
  (and (not (check? state)) (mate? state)))

; special game state: pawn promotion
(defn pawn-up-for-promotion? [{:keys [id piece color]} target-square]
  (let [[_ y] target-square]
    (when (and (= piece :pawn)
               (or (and (= color :white) (= y 1))
                   (and (= color :black) (= y 8))))
      id)))

(defn promote-pawn [{:keys [turn board] :as state} pawn-id promote-to]
  (let [pawn-color (case turn :white :black :black :white)]
    (assert (get (pawn-color board) pawn-id) (str "Pawn " pawn-id " not found in " pawn-color))
    (assoc-in state [:board pawn-color pawn-id :piece] promote-to)))


; special game state: castling
;; long castling
(defn long-castling? [{:keys [turn board] :as state}]
  (let [pieces (turn board)
        king (->> pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  first
                  second)
        king-moved? (:moved? king)
        king-pos (:pos king)
        rooks (->> pieces
                   (filter (fn [[_ {:keys [piece]}]] (= :rook piece)))
                   (map second)
                   (remove :moved?))
        left-rook-pos  (case turn :white [1 8] :black [1 1])
        left-rook  (->> rooks
                        (filter (fn [{:keys [pos]}] (= pos left-rook-pos)))
                        first)
        king-checked? (fn [st] (-> st
                                   (refresh-state)
                                   (check?)))]
    (when (and (not king-moved?) (not (check? state)) left-rook)
      (let [squares-between  (set (case turn :white [[2 8] [3 8] [4 8]] :black [[2 1] [3 1] [4 1]]))
            squares-to-check (set (case turn :white [      [3 8] [4 8]] :black [      [3 1] [4 1]]))
            pieces-in-between? (some (fn [[_ {:keys [pos]}]] (squares-between pos)) pieces)
            future-states (map #(moves/move! king-pos % state) squares-to-check)]
        (and (not pieces-in-between?) (not (some king-checked? future-states)))))))


;; short castling
(defn short-castling? [{:keys [turn board] :as state}]
  (let [pieces (turn board)
        king (->> pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  first
                  second)
        king-moved? (:moved? king)
        king-pos (:pos king)
        rooks (->> pieces
                   (filter (fn [[_ {:keys [piece]}]] (= :rook piece)))
                   (map second)
                   (remove :moved?))
        right-rook-pos (case turn :white [8 8] :black [8 1])
        squares-to-the-right (case turn :white [[6 8] [7 8]] :black [[6 1] [7 1]])
        right-rook (->> rooks
                        (filter (fn [{:keys [pos]}] (= pos right-rook-pos)))
                        first)
        king-checked? (fn [st] (-> st
                                   (refresh-state)
                                   (check?)))]
    (when (and (not king-moved?) (not (check? state)) right-rook)
      (let [squares-between  (set (case turn :white [[6 8] [7 8]] :black [[6 1] [7 1]]))
            squares-to-check squares-between
            pieces-in-between? (some (fn [[_ {:keys [pos]}]] (squares-between pos)) pieces)
            future-states (map #(moves/move! king-pos % state) squares-to-check)]
        (and (not pieces-in-between?) (not (some king-checked? future-states)))))))

(defn add-castling-moves
  "NOTE: has to be run after calculate-all-moves"
  [{:keys [turn board] :as state}]
  (let [pieces (turn board)
        king (->> pieces
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  first
                  second)
        king-id (:id king)
        short-castling-open? (short-castling? state)
        long-castling-open? (long-castling? state)
        short-castling-move (if short-castling-open?
                              (case turn :white #{[7 8]} :black #{[7 1]})
                              #{})
        long-castling-move (if long-castling-open?
                              (case turn :white #{[3 8]} :black #{[3 1]})
                             #{})]
      (-> state
        (assoc-in [:board turn king-id :short-castling?] short-castling-open?)
        (assoc-in [:board turn king-id :long-castling?] long-castling-open?)
        (update-in [:board turn king-id :moves] (fn [s] (s/union s short-castling-move long-castling-move))))))
            

(defn dfs-with-state-update 
  "DFS variation to be able to get all possible moves for a piece if all the other pieces are frozen"
  [graph start neighbors-fn graph-update-fn]
  (loop [g graph
         stack (list start)
         visited #{}]
    (if (not (seq stack))
      visited
      (let [current (first stack)
            visited? (fn [v] (visited v))
            new-g (graph-update-fn g current)]
        (if (visited? current)
          (recur new-g (rest stack) visited)
          (recur 
            new-g
            (reduce conj stack (remove visited? (neighbors-fn new-g current)))
            (conj visited current)))))))


(defn explore-board [{:keys [board turn] :as state} color piece-id]
  (let [state (refresh-state (cond
                               (= turn color) state
                               (not= turn color) (switch-turn state)))
        pieces (color board)
        _ (assert (get pieces piece-id) (str "Piece " piece-id " not found"))
        start (get-in state [:board color piece-id :pos])
        gf (fn [s pos] (let [from (get-in s [:board color piece-id :pos])
                             to pos]
                        (if (= to from)
                          s
                          (-> (make-move from to s)
                              (refresh-state)
                              (filter-out-checked-moves)))))
        nf (fn [g [x y]] (let [from (get-in g [:board color piece-id :pos])
                               to [x y]
                               new-state (gf g to)
                               ns (get-in new-state [:board color piece-id :moves])]
                          (vec ns)))]
   (dfs-with-state-update state start nf gf)))


(defn pawn-deadlock? [{:keys [turn board] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (into black white)
        all-pieces (map second all-pieces)
        is-pawn-fn (fn [piece] (= (:piece piece) :pawn))
        pawns (filter is-pawn-fn all-pieces)
        cant-move? (fn [{:keys [moves attacks]}] (and (empty? moves) (empty? attacks)))]
    (every? cant-move? pawns)))
  

(defn insufficient-material? [{:keys [turn board] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (into black white)
        all-pieces (map second all-pieces)
        piece-kinds (sort (map :piece all-pieces))
        kk?  (= piece-kinds `(        :king :king))
        bkk? (= piece-kinds `(:bishop :king :king))
        hkk? (= piece-kinds `(:king :king :knight))]
    (or kk? bkk? hkk?)))


(defn dead-position? [{:keys [turn board] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (into black white)
        all-pieces (map second all-pieces)
        piece-kinds (sort (map :piece all-pieces))
        ; below is the deadlock related stuff
        is-king-fn (fn [[_ piece]] (= (:piece piece) :king))
        white-king-id (first (first (filter is-king-fn white)))
        black-king-id (first (first (filter is-king-fn black)))
        ; _ (println (explore-board state :white white-king-id))
        ; _ (println (explore-board state :black black-king-id))
        deadlock? (and (= #{:pawn :king} (set piece-kinds))
                       (pawn-deadlock? state)
                       (empty? (s/intersection  ; kings can never reach each other
                                (explore-board state :white white-king-id)
                                (explore-board state :black black-king-id))))]
    (or (insufficient-material? state) deadlock?)))


(defn game-over? [state]
  ; NOTE need to switch the turn because this check is supposed to happen after enemy's turn
  (let [next-state (switch-turn state)]
    (or (checkmate? next-state)
        (stalemate? next-state)  
        (dead-position? next-state))))


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
                               filter-out-checked-moves
                               add-castling-moves)]
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
                    next-state (-> next-state
                                   filter-out-checked-moves
                                   add-castling-moves)]
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

  (mate? (init-game))

  (defn dfs 
    "Pseudo code (taken from https://www.cs.toronto.edu/~heap/270F02/node36.html):
    ---------------------------------------------------------------
    DFS(G,v)   ( v is the vertex where the search starts )
           Stack S := {};   ( start with an empty stack )
           for each vertex u, set visited[u] := false;
           push S, v;
           while (S is not empty) do
              u := pop S;
              if (not visited[u]) then
                 visited[u] := true;
                 for each unvisited neighbour w of u
                    push S, w;
              end if
           end while
        END DFS()
    "
    [graph start neighbors-fn]
    (loop [stack (list start)
           visited #{}]
      (if (not (seq stack))
        visited
        (let [current (first stack)
              visited? (fn [v] (visited v))]
          (if (visited? current)
            (recur (rest stack) visited)
            (recur 
              (reduce conj stack (remove visited? (neighbors-fn graph current)))
              (conj visited current))))))
    (let [[xmax ymax] [3 3]  ;; dfs test on a simple rect lattice with south/north/west/east moves
          g (for [i (range xmax) j (range ymax)] [i j])
          s [0 0]
          gf (fn [g _] g)
          nf (fn [_ [x y]] (let [dxdy (for [i [-1 0 +1] j [-1 0 +1]] [i j])
                                 ns (for [[dx dy] dxdy
                                          :let [[xnew ynew] [(+ x dx) (+ y dy)]]
                                          :when (and
                                                  (not= (abs dx) (abs dy))
                                                  (< -1 xnew) (< xnew xmax)
                                                  (< -1 ynew) (< ynew ymax))] [xnew ynew])]
                             ns))]
      (dfs g s nf))
    
    (let [s1 (explore-board (init-game) :white ":king:white:0")
          s2 (explore-board (init-game) :black ":king:black:0")]
      (s/intersection s1 s2))
    (dead-position? (refresh-state (init-game)))))

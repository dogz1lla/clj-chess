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
    ; (assert (#{0 1} (count checkers)) "More than 1 checker at a time detected")
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
  b) moving to any of those squares will put it in check state.
  NOTE: make sure this is called after all moves and attacks are updated

  Logic
  - get all moves and union with all attacks
  - for each of the positions from that union create a new state where the king is in that position
  - call update-check on each of those hypothetical states
  - return (every? check? [state-1 state-2 ...])
  "
  [{:keys [board turn] :as state}]
  (let [king (->> board
                  (filter second)
                  (filter (fn [[_ {:keys [piece]}]] (= :king piece)))
                  (filter (fn [[_ {:keys [color]}]] (= turn color)))
                  first
                  second)
        king-pos (:pos king)
        future-moves (s/union (:moves king) (:attacks king))
        future-states (map #(moves/move! king-pos % state) future-moves)
        king-checked? (fn [st] (-> st
                                   (calculate-all-moves)
                                   (calculate-all-attacks)
                                   (update-check)
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
        (println "Game over!")
        (let [msg-type (:type msg)
              msg-body (:body msg)
              next-state (case msg-type
                           :game-start state
                           :move (let [[from to] msg-body]
                                   (make-move from to state)))
              next-state (-> next-state
                             (calculate-all-moves)
                             (calculate-all-attacks)
                             (update-check))]
          ; (println (str "Got message type " msg-type ", the msg is " msg))
          (async/>! c-out next-state)
          (recur (async/<! c-in) next-state))))
    [c-in c-out]))


; NEXT: only keep track of occupied squares in the state?
(let [[in out] (run-game!)]
  (println (async/<!! out))
  (async/>!! in {:type :move :body [[1 2] [1 4]]})
  (println (async/<!! out))
  (async/>!! in {:type :move :body [[1 4] [1 5]]})
  (println (async/<!! out))
  (async/>!! in {:type :game-over})
  #_(println (async/<!! out)))

; (let [[in out] (run-game!)]
;   (println (async/<!! out))
;   (async/<!! (async/timeout 1000))
;   (async/>!! in {:type :move})
;   (println (async/<!! out))
;   (async/<!! (async/timeout 1000))
;   (async/>!! in {:type :attack})
;   (println (async/<!! out))
;   (async/<!! (async/timeout 1000))
;   (async/>!! in {:type :game-over})
;   #_(println (async/<!! out)))


{:type :move :body [[1 1] [3 3]]}
{:type :game-over :body nil}
{:type :game-over :body nil}


(defn pipeline-test []
  (let [c (async/chan)]
    (async/go-loop [msg "start"
                    t (async/timeout 2000)]
     (if
       (not= msg "exit")
       (do
         (println msg)
         (async/<! t)
         (recur (async/<! c) (async/timeout (* 1000 (rand-nth [1 2])))))
       nil))
   c))
    
#_(let [c (pipeline-test)]
    (async/>!! c "hello,")
    (async/>!! c "world")
    (async/>!! c "exit"))

#_(doseq [n (range 10) :let [i (-> n
                                   inc
                                   range
                                   rand-nth)]]                    
   (async/go
     (async/<! (async/timeout (* i 1000)))
     (println n)))

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
    (check? state))

  (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
        piece-2 {:piece :pawn :color :black :pos [8, 7] :id "pawn1"}
        piece-3 {:piece :pawn :color :black :pos [7, 6] :id "pawn2"}
        piece-4 {:piece :rook :color :black :pos [1, 7] :id "rook1"}
        board (-> (state/init-board)
                  (state/put-piece-on-board piece-1)
                  (state/put-piece-on-board piece-2)
                  (state/put-piece-on-board piece-3)
                  (state/put-piece-on-board piece-4))
        state (->> {:board board :turn :white}
                   (calculate-all-moves)
                   (calculate-all-attacks)
                   (update-check))]
    (mate? state)))

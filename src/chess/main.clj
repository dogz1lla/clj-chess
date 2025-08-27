; the set of available moves for a piece depends on the piece's position, king and color
; but that would be in an ideal gas situation where none of the pieces see one another
; in practice the possible moves can be blocked by other pieces, friendly or enemy alike
;
; could calculate the ideal gas set of moves
; then having the col of positions of all other pieces reduce the "ideal" set accordingly
;
; on each turn a player can
;   - move a piece to a valid square
;   - capture a piece
(ns chess.main
  (:require [clojure.set :as s]
            [clojure.core.async :as async]
            [quil.core :as q]
            [chess.state :as state]
            [chess.moves :as moves]
            [chess.attacks :as attacks]
            [chess.game :as game]))


(def cell-size 100)

;--------------------------------------------------------------------------------------------------
; drawing
(defn clear-screen
  "Fill the screen with a color."
  []
  (q/background 255 255 255))

(defn setup
  "quil setup function."
  []
  (clear-screen)
  (q/frame-rate 60)
  
  (q/set-state! :image {:pawn   {:white (q/load-image "./resources/imgs/P_w.png")
                                 :black (q/load-image "./resources/imgs/P_b.png")}
                        :king   {:white (q/load-image "./resources/imgs/K_w.png")
                                 :black (q/load-image "./resources/imgs/K_b.png")}
                        :rook   {:white (q/load-image "./resources/imgs/R_w.png")
                                 :black (q/load-image "./resources/imgs/R_b.png")}
                        :bishop {:white (q/load-image "./resources/imgs/B_w.png")
                                 :black (q/load-image "./resources/imgs/B_b.png")}
                        :knight {:white (q/load-image "./resources/imgs/H_w.png")
                                 :black (q/load-image "./resources/imgs/H_b.png")}
                        :queen  {:white (q/load-image "./resources/imgs/Q_w.png")
                                 :black (q/load-image "./resources/imgs/Q_b.png")}}))


(defn chess-square 
  "Draw a single unit piece."
  [x0 y0 a]
  (q/text (str x0 ", " y0) x0 y0)
  (q/rect x0 y0 a a))

(defn move-square 
  [x0 y0 a]
  (q/fill 255 0 0)
  (q/rect x0 y0 a a))

(defn attack-square 
  [x0 y0 a]
  (q/fill 255 255 0)
  (q/rect x0 y0 a a))

(defn selected-piece-square 
  [x0 y0 a]
  (q/fill 0 0 255)
  (q/rect x0 y0 a a))

(defn chess-puck
  "Draw a single unit piece."
  [x0 y0 r piece color]
  (let [im (color (piece (q/state :image)))]
    ; check if image is loaded using function loaded?
    (when (q/loaded? im) (q/image im x0 y0 r r))))

(defn canvas-pos->cell-coord [x y a]
  (let [max-x (* 8 a)
        max-y max-x
        x-in-bounds? (and (< 0 x) (<= x max-x))
        y-in-bounds? (and (< 0 y) (<= y max-y))]
    [(if x-in-bounds? (inc (quot x a)) nil)
     (if y-in-bounds? (inc (quot y a)) nil)]))

(defn grabbed-piece? [{:keys [board turn]} mouse-x mouse-y a r]
  (let [square (canvas-pos->cell-coord mouse-x mouse-y a)
        ; white (:white board)
        ; black (:black board)
        ; all-pieces (map second (into black white))]
        all-pieces (map second (turn board))]
    ; (get board square)
    (first (filter (fn [{:keys [pos]}] (= pos square)) all-pieces))))

(defn draw-chess-board [a]
  (let [cell-coords (for [i (range 8) j (range 8)
                          :let [I (* a i) J (* a j)]
                          :when (even? (+ i j))]
                      [I J])]
    (doseq [[x y] cell-coords] (chess-square x y a))))

(defn draw-chess-pieces [{:keys [board]} a]
  (let [white (:white board)
        black (:black board)
        all-pieces (map second (into black white))
        ; all-pieces (map second occupied-squares)
        piece-types (map :piece all-pieces)
        piece-colors (map :color all-pieces)
        piece-positions (map :pos all-pieces)
        cell-coords (for [[i j] piece-positions :let [I (* a (dec i)) J (* a (dec j))]] [I J])
        iterator (map vector cell-coords piece-types piece-colors)]
    (doseq [[[x y] p c] iterator] (chess-puck x y a p c))))

(defn draw-possible-moves [move-list a]
  (let [cell-coords (for [[i j] move-list :let [I (* a (dec i)) J (* a (dec j))]] [I J])]
    (doseq [[x y] cell-coords] (move-square x y a))))

(defn draw-possible-attacks [attack-list a]
  (let [cell-coords (for [[i j] attack-list :let [I (* a (dec i)) J (* a (dec j))]] [I J])]
    (doseq [[x y] cell-coords] (attack-square x y a))))


(defn render-fn []
  (let [[in out] (game/run-game!)
        piece-selected? (atom nil)
        selection-cooldown? (atom false)
        game-state (atom (async/<!! out))]
    (fn []
      (clear-screen)
      (q/fill 124 149 92)

       ; draw the chess board squares
      (draw-chess-board cell-size)

       ; drawing possible moves:
       ; - attacks
       ; - moves
      (when @piece-selected?
        (draw-possible-moves   (:moves   @piece-selected?) cell-size)
        (draw-possible-attacks (:attacks @piece-selected?) cell-size)
        (let [[x y] (:pos @piece-selected?)]
          (when (and x y)
            (selected-piece-square (* cell-size (dec x)) (* cell-size (dec y)) cell-size))))

      ; moving the piece:
      ; - if the move is a valid attack -> attack!
      ; - else if the move is a valid move -> move!
      (when (and
              @piece-selected?
              (not @selection-cooldown?)
              (= :left (q/mouse-button)))
        (let [square (canvas-pos->cell-coord (q/mouse-x) (q/mouse-y) cell-size)
              possible-moves   (:moves   @piece-selected?)  ; it is a set of vectors
              possible-attacks (:attacks @piece-selected?)  ; it is a set of vectors
              start (:pos @piece-selected?)]
          (if (possible-attacks square)  ; if can attack -> attack!
            (let [msg {:type :attack :body [start square]}]
              (async/>!! in msg)
              (reset! game-state (async/<!! out)))
            (when (possible-moves square)  ; else if can move -> move!
              (let [msg {:type :move :body [start square]}]
                (async/>!! in msg)
                (reset! game-state (async/<!! out))))))
        (reset! piece-selected? nil))

      ; grab a piece by clicking on it
      (when (and
              (not @selection-cooldown?)
              (not @piece-selected?)
              (grabbed-piece? @game-state (q/mouse-x) (q/mouse-y) cell-size 35)
              (= :left (q/mouse-button)))
        (do
          (reset! piece-selected? (grabbed-piece? @game-state (q/mouse-x) (q/mouse-y) cell-size 35))
          (async/go (async/<! (async/timeout 250)) (reset! selection-cooldown? false))  ; selection cooldown of 250ms
          (reset! selection-cooldown? true)))

      ; cancel selection by pressing space key
      (when (and
              @piece-selected?
              (not @selection-cooldown?)
              (q/key-pressed?)
              (= :space (q/key-as-keyword)))
        (reset! piece-selected? nil))

      ; highlight the square on hover
      (let [[x y] (canvas-pos->cell-coord (q/mouse-x) (q/mouse-y) cell-size)]
        (when (and x y)
          (move-square (* cell-size (dec x)) (* cell-size (dec y)) cell-size)))

      ; draw the chess pieces
      (draw-chess-pieces @game-state cell-size)

      ; dev monitoring
      (doseq [[ind capt fn] [[0 "button" q/mouse-button]
                             #_[0 "key-as-keyword" q/key-as-keyword]
                             #_[1 "pressed?" q/mouse-pressed?]
                             [1 "piece-selected?" (fn [] @piece-selected?)]
                             [2 "x" q/mouse-x]
                             [3 "y" q/mouse-y]
                             [4 "grab" (fn [] (grabbed-piece? @game-state (q/mouse-x) (q/mouse-y) cell-size 35))]
                             [5 "moves" (fn [] (when @piece-selected? (:moves @piece-selected?)))]
                             [6 "attacks" (fn [] (when @piece-selected? (:attacks @piece-selected?)))]
                             [7 "square" (fn [] (canvas-pos->cell-coord (q/mouse-x) (q/mouse-y) cell-size))]]]
        (q/text (str capt " " (fn)) 10 (+ (* 20 ind) 20))))))
     

(q/defsketch chess-game
  :title "tetris"
  :settings #(q/smooth 2)
  :setup setup
  :draw (render-fn)
  :features [:keep-on-top]
  :size [(* 8 cell-size) (* 8 cell-size)]
  :on-close #(println "Game over"))

(comment
  (moves/moves {:piece :pawn :pos [1, 2] :color :white} state/test-game-state)
  (moves/moves {:piece :pawn :pos [2, 1] :color :black})
  (attacks/attacks {:piece :pawn :pos [1, 3] :color :white} state/test-game-state)
  (attacks/attacks {:piece :pawn :pos [5, 3] :color :white} state/test-game-state)
  (for [i (range 8) j (range 8) :let [I (* 100 i) J (* 100 j)] :when (or (even? i) (even? j))] [I J])
  ; (grabbed-piece? state/test-game-state 150 350 100 35)
  (grabbed-piece? (state/init-state) 50 150 100 35)
  (swap! piece-selected? not)
  (deref piece-selected?)
  (quot 0 100)
  (keep-indexed #(when %2 %1) [true true true false true])
  (nth [] 3)
  (filterv #(= :white (:color %)) (map second (:board (state/init-state)))))
  

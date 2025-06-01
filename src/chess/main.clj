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
;
; TODO
; - [x] pawn moves
; - [x] pawn attacks
; - [x] board render (quil)
; - [x] pawn coll render (quil)
; - [x] pawn moving by dragging (quil)
; - [x] convert the mouse position to the cell coord
; - [x] highlight the cell on hover
; - [x] can move a piece
; - [x] pawn moving by allowing to teleport it on one of the allowed squares (quil)
; - [x] first move of a pawn can be a leap
; - [x] do not cover the piece with the selection square
; - [x] make the selected piece selection square a different color
; - [x] bug: pawn can jump over enemy piece
; - [x] add attacks
; - [x] add capturing the piece
; - [x] combine drawing attacks and moves in one let
; - [ ] reconsider how the state works
(ns chess.main
  (:require [clojure.set :as s]
            [clojure.core.async :as async]
            [quil.core :as q]
            [chess.state :as state]
            [chess.moves :as moves]
            [chess.attacks :as attacks]))

(def pieces [:pawn :rook :queen :king :knight :bishop])


(defn move-piece [{:keys [color pos] :as p} new-pos {:keys [white black move-history] :as state}]
  (let [ally-pieces (get state color)
        moved (assoc p :pos new-pos)
        new-ally-pieces (mapv (fn [piece] (if (= p piece) (assoc piece :pos new-pos) piece)) ally-pieces)
        new-state (assoc (assoc state color new-ally-pieces) :move-history (conj move-history moved))]
    new-state))


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
  (q/set-state! :image {:pawn {:white (q/load-image "https://upload.wikimedia.org/wikipedia/commons/0/04/Chess_plt60.png")
                               :black (q/load-image "https://upload.wikimedia.org/wikipedia/commons/c/cd/Chess_pdt60.png")}}))

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
    (when (q/loaded? im) (q/image im x0 y0 r r)))
  #_(q/ellipse x0 y0 r r))

(defn grabbed-piece? [{:keys [white black]} mouse-x mouse-y a r]
  (let [all-pieces (into [] (concat white black))
        piece-positions (map :pos all-pieces)
        centers (for [[i j] piece-positions
                      :let [I (* a (dec i)) J (* a (dec j))]]
                  [(+ (* 0.5 a) I) (+ (* 0.5 a) J)])
        dist2 (fn [[x1 y1] [x2 y2]]
                (let [dx  (- x2 x1)
                      dy  (- y2 y1)
                      dx2 (* dx dx)
                      dy2 (* dy dy)]
                  (+ dx2 dy2)))
        close-enough? (fn [c] (< (dist2 c [mouse-x mouse-y]) (* r r)))
        grabbed-piece-idx (first (keep-indexed #(when %2 %1) (map close-enough? centers)))]
    (when grabbed-piece-idx (nth all-pieces grabbed-piece-idx))))

(defn draw-chess-board [a]
  (let [cell-coords (for [i (range 8) j (range 8)
                          :let [I (* a i) J (* a j)]
                          :when (even? (+ i j))]
                      [I J])]
    (doseq [[x y] cell-coords] (chess-square x y a))))

(defn draw-chess-pieces [{:keys [white black]} selected-piece a]
  (let [all-pieces (into [] (concat white black))
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

(defn mouse-pos->cell-coord [a]
  (let [mouse-x (q/mouse-x)
        mouse-y (q/mouse-y)
        max-x (* 8 a)
        max-y max-x
        x-in-bounds? (and (< 0 mouse-x) (<= mouse-x max-x))
        y-in-bounds? (and (< 0 mouse-y) (<= mouse-y max-y))]
    [(if x-in-bounds? (inc (quot mouse-x a)) nil)
     (if y-in-bounds? (inc (quot mouse-y a)) nil)]))

(def piece-selected? (atom nil))
(def selection-cooldown? (atom false))
(def cell-size 100)
;(def game-state (atom state/test-game-state))
(def game-state (atom state/initial-state))

(defn draw
  "quil draw function."
  []
  ;; reload canvas
  (clear-screen)
  (q/fill 124 149 92)

  ; draw the chess board squares
  (draw-chess-board cell-size)

  ; drawing possible moves:
  ; - attacks
  ; - moves
  (when @piece-selected?
    (draw-possible-moves (moves/moves @piece-selected? @game-state) cell-size)
    (draw-possible-attacks (attacks/attacks @piece-selected? @game-state) cell-size)
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
    (let [square (mouse-pos->cell-coord cell-size)
          possible-moves (moves/moves @piece-selected? @game-state)  ; it is a set of vectors
          possible-attacks (attacks/attacks @piece-selected? @game-state)]  ; it is a set of vectors
      (if (possible-attacks square)  ; if can attack -> attack!
        (reset! game-state (attacks/attack! square @piece-selected? @game-state))
        (when (possible-moves square)  ; else if can move -> move!
          (reset! game-state (move-piece @piece-selected? square @game-state)))))
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
  (let [[x y] (mouse-pos->cell-coord cell-size)]
    (when (and x y)
      (move-square (* cell-size (dec x)) (* cell-size (dec y)) cell-size)))

  ; draw the chess pieces
  (draw-chess-pieces @game-state @piece-selected? cell-size)

  ; dev monitoring
  (doseq [[ind capt fn] [[0 "button" q/mouse-button]
                         #_[0 "key-as-keyword" q/key-as-keyword]
                         #_[1 "pressed?" q/mouse-pressed?]
                         [1 "piece-selected?" (fn [] @piece-selected?)]
                         [2 "x" q/mouse-x]
                         [3 "y" q/mouse-y]
                         [4 "grab" (fn [] (grabbed-piece? @game-state (q/mouse-x) (q/mouse-y) cell-size 35))]
                         [5 "moves" (fn [] (when @piece-selected? (moves/moves @piece-selected? @game-state)))]
                         [6 "attacks" (fn [] (when @piece-selected? (attacks/attacks @piece-selected? @game-state)))]
                         [7 "square" (fn [] (mouse-pos->cell-coord cell-size))]]]
    (q/text (str capt " " (fn)) 10 (+ (* 20 ind) 20))))
  

(q/defsketch chess-game
  :title "tetris"
  :settings #(q/smooth 2)
  :setup setup
  :draw draw
  :features [:keep-on-top]
  :size [(* 8 cell-size) (* 8 cell-size)]
  :on-close #(println "Game over"))

(comment
  (moves/moves {:piece :pawn :pos [1, 2] :color :white} state/test-game-state)
  (moves/moves {:piece :pawn :pos [2, 1] :color :black})
  (attacks/attacks {:piece :pawn :pos [1, 3] :color :white} state/test-game-state)
  (attacks/attacks {:piece :pawn :pos [5, 3] :color :white} state/test-game-state)
  (for [i (range 8) j (range 8) :let [I (* 100 i) J (* 100 j)] :when (or (even? i) (even? j))] [I J])
  (grabbed-piece? state/test-game-state 150 350 100 35)
  (swap! piece-selected? not)
  (deref piece-selected?)
  (quot 0 100)
  (keep-indexed #(when %2 %1) [true true true false true])
  (nth [] 3)
  (move-piece {:piece :pawn :pos [4, 2] :color :white :leap true} [1 1] state/test-game-state))
  

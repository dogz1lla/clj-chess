(ns chess.state)


(defn put-piece-on-board
  "Useful for testing: gives you a board with the piece and also lets you keep the piece handle"
  [board {:keys [color id] :as piece}]
  (assoc-in board [color id] piece))

(defn init-pieces [board {:keys [kind white black]}]
  (let [add-piece (fn [color]
                    (fn [s [idx pos]]
                      (let [piece-id (str kind color ":" idx)]
                        (assoc-in s [color piece-id] {:piece kind :pos pos :color color :id piece-id}))))
        enum-white-positions (map vector (range) white)
        enum-black-positions (map vector (range) black)]
      (reduce
        (add-piece :black)
        (reduce
          (add-piece :white)
          board
          enum-white-positions)
        enum-black-positions)))

; (defn init-board []
;   (let [squares (for [col (range 1 9) row (range 1 9)] [col row])
;         nils (repeat (count squares) nil)
;         board (zipmap squares nils)]
;     board))

; (defn init-state []
;   {:board (-> (init-board)
;               (init-pieces {:kind :king   :white [[5 8]]       :black [[5 1]]})
;               (init-pieces {:kind :queen  :white [[4 8]]       :black [[4 1]]})
;               (init-pieces {:kind :rook   :white [[1 8] [8 8]] :black [[1 1] [8 1]]})
;               (init-pieces {:kind :bishop :white [[3 8] [6 8]] :black [[3 1] [6 1]]})
;               (init-pieces {:kind :knight :white [[2 8] [7 8]] :black [[2 1] [7 1]]})
;               (init-pieces {:kind :pawn   :white (for [x (range 1 9)] [x 7]) :black (for [x (range 1 9)] [x 2])}))
;    :turn :white
;    :history []
;    :captured []})

(defn init-state
  ([]
   {:board (-> {}
               (init-pieces {:kind :king   :white [[5 8]]       :black [[5 1]]})
               (init-pieces {:kind :queen  :white [[4 8]]       :black [[4 1]]})
               (init-pieces {:kind :rook   :white [[1 8] [8 8]] :black [[1 1] [8 1]]})
               (init-pieces {:kind :bishop :white [[3 8] [6 8]] :black [[3 1] [6 1]]})
               (init-pieces {:kind :knight :white [[2 8] [7 8]] :black [[2 1] [7 1]]})
               (init-pieces {:kind :pawn   :white (for [x (range 1 9)] [x 7]) :black (for [x (range 1 9)] [x 2])}))
    :turn :white})
  ([pieces]
   {:board (reduce init-pieces {} pieces)
    :turn :white}))
                 
(comment
  ; (init-board)
  (init-state)
  (init-state [{:kind :king  :white [[5 8]] :black [[5 1]]}
               {:kind :queen :white [[4 8]] :black [[4 1]]}]))

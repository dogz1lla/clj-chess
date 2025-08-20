(ns chess.state)


(def test-game-state {:white [{:piece :pawn :pos [2, 4] :color :white :leap true}
                              {:piece :pawn :pos [4, 5] :color :white :leap true}]
                      :black [{:piece :pawn :pos [2, 5] :color :black :leap true}]
                      :captured-pieces []
                      :move-history [{:piece :pawn :pos [2, 5] :color :black :leap true}]})

(def initial-state {:white [{:piece :pawn :pos [1, 7] :color :white :leap false}
                            {:piece :pawn :pos [2, 7] :color :white :leap false}
                            {:piece :pawn :pos [3, 7] :color :white :leap false}
                            {:piece :pawn :pos [4, 7] :color :white :leap false}
                            {:piece :pawn :pos [5, 7] :color :white :leap false}
                            {:piece :pawn :pos [6, 7] :color :white :leap false}
                            {:piece :pawn :pos [7, 7] :color :white :leap false}
                            {:piece :pawn :pos [8, 7] :color :white :leap false}]
                    :black [{:piece :pawn :pos [1, 2] :color :black :leap false}
                            {:piece :pawn :pos [2, 2] :color :black :leap false}
                            {:piece :pawn :pos [3, 2] :color :black :leap false}
                            {:piece :pawn :pos [4, 2] :color :black :leap false}
                            {:piece :pawn :pos [5, 2] :color :black :leap false}
                            {:piece :pawn :pos [6, 2] :color :black :leap false}
                            {:piece :pawn :pos [7, 2] :color :black :leap false}
                            {:piece :pawn :pos [8, 2] :color :black :leap false}]
                    :captured-pieces []
                    :move-history []})

(defn put-piece-on-board [board {:keys [pos] :as piece}]
  (assoc board pos piece))

(defn init-pieces [board {:keys [kind white black]}]
  (let [add-piece (fn [color] (fn [s pos] (assoc s pos {:piece kind :pos pos :color color})))]
      (reduce
        (add-piece :black)
        (reduce
          (add-piece :white)
          board
          white)
        black)))

(defn init-board []
  (let [squares (for [col (range 1 9) row (range 1 9)] [col row])
        nils (repeat (count squares) nil)
        board (zipmap squares nils)]
    board))

(defn init-state []
  {:board (-> (init-board)
              (init-pieces {:kind :king   :white [[5 8]]       :black [[5 1]]})
              (init-pieces {:kind :queen  :white [[4 8]]       :black [[4 1]]})
              (init-pieces {:kind :rook   :white [[1 8] [8 8]] :black [[1 1] [8 1]]})
              (init-pieces {:kind :bishop :white [[3 8] [6 8]] :black [[3 1] [6 1]]})
              (init-pieces {:kind :knight :white [[2 8] [7 8]] :black [[2 1] [7 1]]})
              (init-pieces {:kind :pawn   :white (for [x (range 1 9)] [x 7]) :black (for [x (range 1 9)] [x 2])}))
   :turn :white
   :history []
   :captured []})

(comment
  (init-board)
  (init-state))
  
(comment
  (put-piece-on-board (init-board) (init-piece :pawn [1 1] :white)))

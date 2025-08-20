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

(defn init-piece [kind pos color]
  {:piece kind :pos pos :color color})

(defn put-piece-on-board [board {:keys [pos] :as piece}]
  (assoc board pos piece))

(defn init-pawns [board]
  (let [white-pos (for [x (range 1 9)] [x 7])
        black-pos (for [x (range 1 9)] [x 2])
        add-pawn (fn [color] (fn [s pos] (assoc s pos (init-piece :pawn pos color))))]
    (reduce
      (add-pawn :black)
      (reduce
        (add-pawn :white)
        board
        white-pos)
      black-pos)))

(defn init-rooks [board]
  (let [white-pos [[1 8] [8 8]]
        black-pos [[1 1] [8 1]]
        add-rook (fn [color] (fn [s pos] (assoc s pos (init-piece :rook pos color))))]
    (reduce
      (add-rook :black)
      (reduce
        (add-rook :white)
        board
        white-pos)
      black-pos)))

(defn init-bishops [board]
  (let [white-pos [[3 8] [6 8]]
        black-pos [[3 1] [6 1]]
        add-bishop (fn [color] (fn [s pos] (assoc s pos (init-piece :bishop pos color))))]
    (reduce
      (add-bishop :black)
      (reduce
        (add-bishop :white)
        board
        white-pos)
      black-pos)))

(defn init-kings [board]
  (let [white-pos [5 8]
        black-pos [5 1]]
    (-> board
        (assoc white-pos (init-piece :king white-pos :white))
        (assoc black-pos (init-piece :king black-pos :black)))))

(defn init-knights [board]
  (let [white-pos [[2 8] [7 8]]
        black-pos [[2 1] [7 1]]
        add-knight (fn [color] (fn [s pos] (assoc s pos (init-piece :knight pos color))))]
    (reduce
      (add-knight :black)
      (reduce
        (add-knight :white)
        board
        white-pos)
      black-pos)))

(defn init-board []
  (let [squares (for [col (range 1 9) row (range 1 9)] [col row])
        nils (repeat (count squares) nil)
        board (zipmap squares nils)]
    board))

(defn init-state []
  {:board (->> (init-board)
               init-pawns
               init-kings
               init-rooks
               init-bishops
               init-knights)
   :turn :white
   :history []
   :captured []})

(comment
  (init-board)
  (init-state))
  
(comment
  (put-piece-on-board (init-board) (init-piece :pawn [1 1] :white)))

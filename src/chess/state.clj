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
                            

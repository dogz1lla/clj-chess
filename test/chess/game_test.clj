(ns chess.game-test
  (:require [clojure.test :as t]
            [chess.moves  :as m]
            [chess.state  :as s]
            [chess.game   :as g]))

(t/deftest game-states
  (t/testing "check states"
    (t/testing "white king no check"
      (t/is (not (let [piece-1 {:piece :king :color :white :pos [3, 3] :id "king1"}
                       piece-2 {:piece :pawn :color :black :pos [3, 2] :id "pawn1"}
                       board (-> (s/init-board)
                                 (s/put-piece-on-board piece-1)
                                 (s/put-piece-on-board piece-2))
                       state (->> {:board board :turn :white}
                                  (g/calculate-all-moves)
                                  (g/calculate-all-attacks)
                                  (g/update-check))]
                     (g/check? state)))))
    (t/testing "white king check"
      (t/is (let [piece-1 {:piece :king :color :white :pos [3, 3] :id "king1"}
                  piece-2 {:piece :pawn :color :black :pos [2, 2] :id "pawn1"}
                  board (-> (s/init-board)
                            (s/put-piece-on-board piece-1)
                            (s/put-piece-on-board piece-2))
                  state (->> {:board board :turn :white}
                             (g/calculate-all-moves)
                             (g/calculate-all-attacks)
                             (g/update-check))]
                (g/check? state)))))

  (t/testing "mate states"
    (t/testing "white king cornered, mate"
      (t/is (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
                  piece-2 {:piece :pawn :color :black :pos [8, 7] :id "pawn1"}
                  piece-3 {:piece :pawn :color :black :pos [7, 6] :id "pawn2"}
                  piece-4 {:piece :rook :color :black :pos [1, 7] :id "rook1"}
                  board (-> (s/init-board)
                            (s/put-piece-on-board piece-1)
                            (s/put-piece-on-board piece-2)
                            (s/put-piece-on-board piece-3)
                            (s/put-piece-on-board piece-4))
                  state (->> {:board board :turn :white}
                             (g/calculate-all-moves)
                             (g/calculate-all-attacks)
                             (g/update-check))]
              (g/mate? state))))))

(t/run-tests)


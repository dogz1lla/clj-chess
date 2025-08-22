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
                (g/check? state))))))

(t/run-tests)

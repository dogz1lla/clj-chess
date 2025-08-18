(ns chess.attacks-test
  (:require [clojure.test :as t]
            [chess.attacks :as a]
            [chess.state :as s]))

(t/deftest pawn-attacks
  (t/testing "white pawns"
    (t/testing "targets on both diagonals"
      (t/is (= #{[2, 2] [4, 2]} (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
                                      other   {:piece :pawn :color :black :pos [2, 2]}
                                      another {:piece :pawn :color :black :pos [4, 2]}
                                      state   {:board {[3 3] pawn [2 2] other [4 2] another} :history [pawn]}]
                                 (a/attacks pawn state))))))
  (t/testing "white kings"
    (t/testing "surrounded, in the corner, nowhere to run"
       (t/is (= #{[1, 2] [2, 2] [2, 1]} (let [king   {:piece :king :pos [1, 1] :color :white}
                                              pawn-1 {:piece :pawn :pos [1, 2] :color :black}
                                              pawn-2 {:piece :pawn :pos [2, 2] :color :black}
                                              pawn-3 {:piece :pawn :pos [2, 1] :color :black}
                                              board (-> (s/init-board)
                                                      (s/put-piece-on-board pawn-1)
                                                      (s/put-piece-on-board pawn-2)
                                                      (s/put-piece-on-board pawn-3))
                                              state {:board board}]
                                         (a/attacks king state)))))))
(t/run-tests)

(ns chess.attacks-test
  (:require [clojure.test :as t]
            [chess.attacks :as a]))

(t/deftest pawn-attacks
  (t/testing "white pawns"
    (t/testing "targets on both diagonals"
      (t/is (= #{[2, 2] [4, 2]} (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
                                      other   {:piece :pawn :color :black :pos [2, 2]}
                                      another {:piece :pawn :color :black :pos [4, 2]}
                                      state   {:board {[3 3] pawn [2 2] other [4 2] another} :history [pawn]}]
                                    (a/attacks pawn state)))))))

(t/run-tests)

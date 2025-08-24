(ns chess.attacks-test
  (:require [clojure.test :as t]
            [chess.attacks :as a]
            [chess.state :as s]))

(t/deftest pawn-attacks
  (t/testing "white pawns"
    (t/testing "targets on both diagonals"
      (t/is (= #{[2, 2] [4, 2]} (let [pawn    {:piece :pawn :pos [3, 3] :color :white :id "pawn:white:1"}
                                      other   {:piece :pawn :pos [2, 2] :color :black :id "pawn:black:1"}
                                      another {:piece :pawn :pos [4, 2] :color :black :id "pawn:black:2"}
                                      board (-> {}
                                                (s/put-piece-on-board pawn)
                                                (s/put-piece-on-board other)
                                                (s/put-piece-on-board another))
                                      state {:board board}]
                                   (a/attacks pawn state)))))))

(t/deftest king-attacks
  (t/testing "white kings"
    (t/testing "surrounded, in the corner, nowhere to run"
       (t/is (= #{[1, 2] [2, 2] [2, 1]} (let [king   {:piece :king :pos [1, 1] :color :white :id "king:white:1"}
                                              pawn-1 {:piece :pawn :pos [1, 2] :color :black :id "pawn:black:1"}
                                              pawn-2 {:piece :pawn :pos [2, 2] :color :black :id "pawn:black:2"}
                                              pawn-3 {:piece :pawn :pos [2, 1] :color :black :id "pawn:black:3"}
                                              board (-> {}
                                                        (s/put-piece-on-board king)
                                                        (s/put-piece-on-board pawn-1)
                                                        (s/put-piece-on-board pawn-2)
                                                        (s/put-piece-on-board pawn-3))
                                              state {:board board}]
                                         (a/attacks king state)))))))
(t/run-tests)

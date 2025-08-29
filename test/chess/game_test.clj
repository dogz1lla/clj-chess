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
                       board (-> {}
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
                  board (-> {}
                            (s/put-piece-on-board piece-1)
                            (s/put-piece-on-board piece-2))
                  state (->> {:board board :turn :white}
                             (g/calculate-all-moves)
                             (g/calculate-all-attacks)
                             (g/update-check))]
                (g/check? state))))

    (t/testing "potential check restricts moves"
      (t/is (= #{[2 1]}
               (let [king {:piece :king :pos [1, 1] :color :white :id "king:white:0"}
                     rook {:piece :rook :pos [8, 2] :color :black :id "rook:black:0"}
                     board (-> {}
                               (s/put-piece-on-board king)
                               (s/put-piece-on-board rook))
                     state (-> {:board board :turn :white}
                               (g/refresh-state)
                               (g/remove-invalid-king-moves))]
                 (get-in state [:board :white "king:white:0" :moves])))))

    (t/testing "potential check restricts attacks"
      (t/is (= #{}
               (let [king {:piece :king :pos [1, 1] :color :white :id "king:white:0"}
                     rook {:piece :rook :pos [8, 2] :color :black :id "rook:black:0"}
                     pawn {:piece :rook :pos [2, 2] :color :black :id "pawn:black:0"}
                     board (-> {}
                               (s/put-piece-on-board king)
                               (s/put-piece-on-board rook)
                               (s/put-piece-on-board pawn))
                     state (-> {:board board :turn :white}
                               (g/refresh-state)
                               (g/remove-invalid-king-moves))]
                 (get-in state [:board :white "king:white:0" :attacks]))))))

  (t/testing "mate states"
    (t/testing "start of the game, no mate"
      (t/is (not (g/mate? (g/init-game)))))

    (t/testing "white king cornered, no mate"
      (t/is (not (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
                       piece-2 {:piece :pawn :color :black :pos [8, 7] :id "pawn1"}
                       piece-3 {:piece :pawn :color :black :pos [7, 6] :id "pawn2"}
                       piece-4 {:piece :rook :color :black :pos [1, 1] :id "rook1"}
                       board (-> {}
                                 (s/put-piece-on-board piece-1)
                                 (s/put-piece-on-board piece-2)
                                 (s/put-piece-on-board piece-3)
                                 (s/put-piece-on-board piece-4))
                       state (->> {:board board :turn :white}
                                  (g/calculate-all-moves)
                                  (g/calculate-all-attacks)
                                  (g/update-check))]
                   (g/mate? state)))))

    (t/testing "white king cornered, mate"
      (t/is (let [piece-1 {:piece :king :color :white :pos [8, 8] :id "king1"}
                  piece-2 {:piece :pawn :color :black :pos [8, 7] :id "pawn1"}
                  piece-3 {:piece :pawn :color :black :pos [7, 6] :id "pawn2"}
                  piece-4 {:piece :rook :color :black :pos [1, 7] :id "rook1"}
                  board (-> {}
                            (s/put-piece-on-board piece-1)
                            (s/put-piece-on-board piece-2)
                            (s/put-piece-on-board piece-3)
                            (s/put-piece-on-board piece-4))
                  state (->> {:board board :turn :white}
                             (g/calculate-all-moves)
                             (g/calculate-all-attacks)
                             (g/update-check))]
              (g/mate? state))))

    (t/testing "there is a check and nowhere to go but a rook can step in -- no mate"
      (t/is (not (let [piece-1 {:piece :king :color :white :pos [1, 1] :id "king1"}
                       piece-2 {:piece :rook :color :white :pos [2, 2] :id "rook1"}
                       piece-3 {:piece :rook :color :black :pos [8, 1] :id "rook2"}
                       piece-4 {:piece :bishop :color :black :pos [3, 4] :id "bishop1"}
                       board (-> {}
                                 (s/put-piece-on-board piece-1)
                                 (s/put-piece-on-board piece-2)
                                 (s/put-piece-on-board piece-3)
                                 (s/put-piece-on-board piece-4))
                       state (->> {:board board :turn :white}
                                  (g/calculate-all-moves)
                                  (g/calculate-all-attacks)
                                  (g/update-check))]
                   (g/mate? state)))))

    (t/testing "there is a check and nowhere to go but a rook capture the checker -- no mate"
      (t/is (not (let [piece-1 {:piece :king :color :white :pos [1, 1] :id "king1"}
                       piece-2 {:piece :rook :color :white :pos [8, 8] :id "rook1"}
                       piece-3 {:piece :rook :color :black :pos [8, 1] :id "rook2"}
                       piece-4 {:piece :bishop :color :black :pos [3, 4] :id "bishop1"}
                       piece-5 {:piece :pawn :color :white :pos [2, 2] :id "pawn1"}
                       board (-> {}
                                 (s/put-piece-on-board piece-1)
                                 (s/put-piece-on-board piece-2)
                                 (s/put-piece-on-board piece-3)
                                 (s/put-piece-on-board piece-4)
                                 (s/put-piece-on-board piece-5))
                       state (->> {:board board :turn :white}
                                  (g/calculate-all-moves)
                                  (g/calculate-all-attacks)
                                  (g/update-check))]
                   (g/mate? state)))))))

(t/run-tests)

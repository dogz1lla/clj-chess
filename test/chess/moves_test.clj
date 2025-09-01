(ns chess.moves-test
  (:require [clojure.test :as t]
            [chess.moves :as m]
            [chess.state :as s]))


(t/deftest sanity-checks
  (t/testing "pawns"
    (t/testing "first move, no obstacles"
      (t/is (= #{[1, 6] [1, 5]} (let [pawn  {:piece :pawn :pos [1, 7] :color :white :id "pawn:white:0"}
                                      board (-> {}
                                                (s/put-piece-on-board pawn))
                                      state {:board board}]
                                   (m/moves pawn state)))))))

(t/deftest pawn-moves
  (t/testing "white pawns"
    (t/testing "first move, no obstacles"
      (t/is (= #{[1, 6] [1, 5]} (let [pawn  {:piece :pawn :pos [1, 7] :color :white :id "pawn:white:0"}
                                      board (-> {}
                                                (s/put-piece-on-board pawn))
                                      state {:board board}]
                                   (m/moves pawn state)))))
    (t/testing "first move, leap blocked"
      (t/is (= #{[1, 6]} (let [pawn     {:piece :pawn :pos [1, 7] :color :white :id "pawn:white:0"}
                               obstacle {:piece :pawn :pos [1, 5] :color :white :id "pawn:white:1"}
                               board (-> {}
                                         (s/put-piece-on-board pawn)
                                         (s/put-piece-on-board obstacle))
                               state {:board board}]
                            (m/moves pawn state)))))
    (t/testing "first move, full blocked"
      (t/is (= #{} (let [pawn     {:piece :pawn :pos [1, 7] :color :white :id "pawn:white:0"}
                         obstacle {:piece :pawn :pos [1, 6] :color :white :id "pawn:white:1"}
                         board (-> {}
                                   (s/put-piece-on-board pawn)
                                   (s/put-piece-on-board obstacle))
                         state {:board board}]
                      (m/moves pawn state)))))
    (t/testing "at the edge"
      (t/is (= #{} (let [pawn  {:piece :pawn :pos [1, 1] :color :white :id "pawn:white:0"}
                         board (-> {}
                                   (s/put-piece-on-board pawn))
                         state {:board board}]
                      (m/moves pawn state)))))
    (t/testing "no out of bounds moves"
      (not (some (fn [[x y]] (or (neg? x) (< 8 x) (neg? y) (< 8 y)))
                 (let [pawn  {:piece :pawn :pos [1, 7] :color :white :id "pawn:white:0"}
                       board (-> {}
                                 (s/put-piece-on-board pawn))
                       state {:board board}]
                     (m/moves pawn state))))))

  (t/testing "black pawns"
    (t/testing "first move, no obstacles"
      (t/is (= #{[1, 3] [1, 4]} (let [pawn  {:piece :pawn :pos [1, 2] :color :black :id "pawn:black:0"}
                                      state {:board {[1 2] pawn}}
                                      board (-> {}
                                                (s/put-piece-on-board pawn))
                                      state {:board board}]
                                  (m/moves pawn state)))))
    (t/testing "first move, leap blocked"
      (t/is (= #{[1, 3]} (let [pawn     {:piece :pawn :pos [1, 2] :color :black :id "pawn:black:0"}
                               obstacle {:piece :pawn :pos [1, 4] :color :black :id "pawn:black:1"}
                               board (-> {}
                                         (s/put-piece-on-board pawn)
                                         (s/put-piece-on-board obstacle))
                               state {:board board}]
                            (m/moves pawn state)))))
    (t/testing "first move, full blocked"
      (t/is (= #{} (let [pawn     {:piece :pawn :pos [1, 2] :color :black :id "pawn:white:0"}
                         obstacle {:piece :pawn :pos [1, 3] :color :black :id "pawn:white:1"}
                         board (-> {}
                                   (s/put-piece-on-board pawn)
                                   (s/put-piece-on-board obstacle))
                         state {:board board}]
                      (m/moves pawn state)))))
    (t/testing "at the edge"
      (t/is (= #{} (let [pawn  {:piece :pawn :pos [1, 8] :color :black :id "pawn:black:0"}
                         board (-> {}
                                   (s/put-piece-on-board pawn))
                         state {:board board}]
                      (m/moves pawn state)))))))

(t/deftest king-moves
  (t/testing "white kings"
    (t/testing "nobody around"
      (t/is (= #{[4 5] [6 5] [5 4] [5 6] [4 4] [4 6] [6 4] [6 6]}
               (let [king {:piece :king :pos [5, 5] :color :white :id "king:white:0"}
                     board (-> {}
                               (s/put-piece-on-board king))
                     state {:board board}]
                 (m/moves king state)))))
    (t/testing "nobody around, on the edge"
      (t/is (= #{[1 1] [1 2] [2 2] [3 2] [3 1]} (let [king {:piece :king :pos [2, 1] :color :white :id "king:white:0"}
                                                      board (-> {}
                                                                (s/put-piece-on-board king))
                                                      state {:board board}]
                                                  (m/moves king state)))))
    (t/testing "nobody around, in the corner"
          (t/is (= #{[1 2] [2 2] [2 1]}
                   (let [king {:piece :king :pos [1, 1] :color :white :id "king:white:0"}
                         board (-> {}
                                   (s/put-piece-on-board king))
                         state {:board board}]
                     (m/moves king state)))))
    (t/testing "a friendly piece in the nearest shell"
      (t/is (= #{[6 5] [5 4] [5 6] [4 4] [4 6] [6 4] [6 6]}
               (let [king {:piece :king :pos [5, 5] :color :white :id "king:white:0"}
                     pawn {:piece :pawn :pos [4, 5] :color :white :id "pawn:white:0"}
                     board (-> {}
                               (s/put-piece-on-board king)
                               (s/put-piece-on-board pawn))
                     state {:board board}]
                  (m/moves king state)))))
    (t/testing "an opponent piece in the nearest shell"
      (t/is (= #{[6 5] [5 4] [5 6] [4 4] [4 6] [6 4] [6 6]}
               (let [king {:piece :king :pos [5, 5] :color :white :id "king:white:0"}
                     pawn {:piece :pawn :pos [4, 5] :color :black :id "pawn:black:0"}
                     board (-> {}
                               (s/put-piece-on-board king)
                               (s/put-piece-on-board pawn))
                     state {:board board}]
                  (m/moves king state)))))
    (t/testing "surrounded, in the corner, nowhere to run"
          (t/is (= #{} (let [king   {:piece :king :pos [1, 1] :color :white :id "king:white:0"}
                             pawn-1 {:piece :pawn :pos [1, 2] :color :black :id "pawn:black:0"}
                             pawn-2 {:piece :pawn :pos [2, 2] :color :black :id "pawn:black:1"}
                             pawn-3 {:piece :pawn :pos [2, 1] :color :black :id "pawn:black:2"}
                             board (-> {}
                                       (s/put-piece-on-board king)
                                       (s/put-piece-on-board pawn-1)
                                       (s/put-piece-on-board pawn-2)
                                       (s/put-piece-on-board pawn-3))
                             state {:board board}]
                         (m/moves king state)))))
    (t/testing "nobody around, on the edge"
      (t/is (= #{[1 1] [1 2] [2 2] [3 2] [3 1]} (let [king {:piece :king :pos [2, 1] :color :white :id "king:white:0"}
                                                      board (-> {}
                                                                (s/put-piece-on-board king))
                                                      state {:board board}]
                                                  (m/moves king state)))))

    (t/testing "no out of bounds moves"
      (not (some (fn [[x y]] (or (neg? x) (< 8 x) (neg? y) (< 8 y)))
                 (let [king  {:piece :king :pos [1, 1] :color :white :id "king:white:0"}
                       board (-> {}
                                 (s/put-piece-on-board king))
                       state {:board board}]
                     (m/moves king state)))))
    (t/testing "no out of bounds moves 2"
      (not (some (fn [[x y]] (or (neg? x) (< 8 x) (neg? y) (< 8 y)))
                 (let [king  {:piece :king :pos [8, 8] :color :white :id "king:white:0"}
                       board (-> {}
                                 (s/put-piece-on-board king))
                       state {:board board}]
                     (m/moves king state)))))))

(t/run-tests)

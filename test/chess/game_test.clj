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
                               (g/filter-out-checked-moves))]
                 (get-in state [:board :white "king:white:0" :moves])))))

    (t/testing "potential check restricts moves 2"
      (t/is (= #{[3 8] [5 7] [3 7]}
               (let [king   {:piece :king   :pos [4, 8] :color :white :id "king:white:0"}
                     knight {:piece :knight :pos [6, 6] :color :black :id "knight:black:0"}
                     board (-> {}
                               (s/put-piece-on-board king)
                               (s/put-piece-on-board knight))
                     state (-> {:board board :turn :white}
                               (g/refresh-state)
                               (g/filter-out-checked-moves))]
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
                               (g/filter-out-checked-moves))]
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
                   (g/mate? state))))))

  (t/testing "castling"
    (t/testing "castling available"
      (let [pieces [{:kind :king :white [[5 8]]       :black [[5 1]]}
                    {:kind :rook :white [[1 8] [8 8]] :black [[1 1] [8 1]]}]
            state (s/init-state pieces)]
        (t/testing "short, white"
          (t/is (g/short-castling? state)))
        (t/testing "long, white"
          (t/is (g/long-castling? state)))
        (t/testing "short, black"
          (t/is (g/short-castling? (g/switch-turn state))))
        (t/testing "long, black"
          (t/is (g/long-castling? (g/switch-turn state))))))

    (t/testing "castling available, but piece moved"
      (let [pieces [{:kind :king :white [[5 8]]       :black [[5 1]]}
                    {:kind :rook :white [[1 8] [8 8]] :black [[1 1] [8 1]]}]
            state (s/init-state pieces)]
        (t/testing "white king moved"
          (t/is (not (-> state
                         (assoc-in [:board :white ":king:white:0" :moved?] true)
                         (g/short-castling?)))))
        (t/testing "white rook moved (right)"
          (t/is (not (-> state
                         (assoc-in [:board :white ":rook:white:1" :moved?] true)
                         (g/short-castling?)))))
        (t/testing "white rook moved (left)"
          (t/is (not (-> state
                         (assoc-in [:board :white ":rook:white:0" :moved?] true)
                         (g/long-castling?)))))
        (t/testing "black king moved"
          (t/is (not (-> state
                         (assoc-in [:board :black ":king:black:0" :moved?] true)
                         (g/switch-turn)
                         (g/short-castling?)))))
        (t/testing "black rook moved (right)"
          (t/is (not (-> state
                         (assoc-in [:board :black ":rook:black:1" :moved?] true)
                         (g/switch-turn)
                         (g/short-castling?)))))
        (t/testing "black rook moved (left)"
          (t/is (not (-> state
                         (assoc-in [:board :black ":rook:black:0" :moved?] true)
                         (g/switch-turn)
                         (g/long-castling?)))))))
    (t/testing "castling available, but one of the squares is checked"
      (let [pieces [{:kind :king :white [[5 8]]       :black [[5 1]]}
                    {:kind :rook :white [[1 8] [8 8]] :black [[1 1] [8 1]]}
                    {:kind :pawn :white [[5 2]] :black [[5 7]]}]
            state (s/init-state pieces)]
        (t/testing "king's short move is checked, white"
          (t/is (not (-> state
                         (g/refresh-state)
                         (g/short-castling?)))))
        (t/testing "king's long move is checked, white"
          (t/is (not (-> state
                         (g/refresh-state)
                         (g/long-castling?)))))
        (t/testing "king's short move is checked, black"
          (t/is (not (-> state
                         (g/refresh-state)
                         (g/switch-turn)
                         (g/short-castling?)))))
        (t/testing "king's long move is checked, black"
          (t/is (not (-> state
                         (g/refresh-state)
                         (g/switch-turn)
                         (g/long-castling?)))))))
    (t/testing "castling available, but king is checked"
      (let [pieces [{:kind :king :white [[5 8]]       :black [[5 1]]}
                    {:kind :rook :white [[1 8] [8 8]] :black [[1 1] [8 1]]}
                    {:kind :pawn :white [[4 2]] :black [[4 7]]}]
            state (s/init-state pieces)]
        (t/testing "short, white"
          (t/is (not (-> state
                         (g/refresh-state)
                         (g/short-castling?)))))
        (t/testing "long, white"
          (t/is (not (-> state
                         (g/refresh-state)
                         (g/long-castling?)))))
        (t/testing "short, black"
          (t/is (not (-> state
                         (g/switch-turn)
                         (g/refresh-state)
                         (g/short-castling?)))))
        (t/testing "long, black"
          (t/is (not (-> state
                         (g/switch-turn)
                         (g/refresh-state)
                         (g/long-castling?))))))))
  (t/testing "game over states"
    (t/testing "pawn deadlock"
      (t/is (let [pieces [{:kind :pawn 
                           :white [[1 5]]
                           :black [[1 4]]}]
                  state (-> pieces
                            (s/init-state)
                            (g/refresh-state))]
              (g/pawn-deadlock? state))))
    (t/testing "pawn deadlock? not really, there is an attack"
      (t/is (not (let [pieces [{:kind :pawn 
                                :white [[1 5] [2 5]]
                                :black [[1 4] [2 4]]}]
                       state (-> pieces
                                 (s/init-state)
                                 (g/refresh-state))]
                   (g/pawn-deadlock? state)))))
    (t/testing "pawn deadlock? yep"
      (t/is (let [pieces [{:kind :pawn 
                           :white [[1 5] [3 5]]
                           :black [[1 4] [3 4]]}]
                  state (-> pieces
                            (s/init-state)
                            (g/refresh-state))]
              (g/pawn-deadlock? state))))
    (t/testing "insufficient material, two kings"
      (t/is (let [pieces [{:kind :king :white [[1 8]] :black [[1 1]]}]
                  state (-> pieces
                            (s/init-state)
                            (g/refresh-state))]
              (g/insufficient-material? state))))
    (t/testing "insufficient material, two kings and a bishop"
      (t/is (let [pieces [{:kind :king :white [[1 8]] :black [[1 1]]}
                          {:kind :bishop :white [] :black [[3 1]]}]
                  state (-> pieces
                            (s/init-state)
                            (g/refresh-state))]
              (g/insufficient-material? state))))
    (t/testing "insufficient material, two kings and a knight"
      (t/is (let [pieces [{:kind :king :white [[1 8]] :black [[1 1]]}
                          {:kind :knight :white [] :black [[3 1]]}]
                  state (-> pieces
                            (s/init-state)
                            (g/refresh-state))]
              (g/insufficient-material? state))))
    (t/testing "sufficient material: two kings and two knights"
      (t/is (not (let [pieces [{:kind :king :white [[1 8]] :black [[1 1]]}
                               {:kind :knight :white [] :black [[3 1] [5 1]]}]
                       state (-> pieces
                                 (s/init-state)
                                 (g/refresh-state))]
                   (g/insufficient-material? state)))))
    (t/testing "dead position"
      (t/is (let [pieces [{:kind :king :white [[1 8]] :black [[1 1]]}
                          {:kind :pawn
                           :white [[1 5] [3 5] [5 5] [7 5]]
                           :black [[1 4] [3 4] [5 4] [7 4]]}]
                  state (-> pieces
                            (s/init-state)
                            (g/refresh-state))]
              (g/dead-position? state))))))
              

(t/run-tests)

(ns chess.moves-test
  (:require [clojure.test :as t]
            [chess.moves :as m]))


(t/deftest pawn-moves
  (t/testing "white pawns"
    (t/testing "first move, no obstacles"
      (t/is (= #{[1, 6] [1, 5]} (let [pawn  {:piece :pawn :pos [1, 7] :color :white}
                                      state {:white [pawn] :black []}]
                                   (m/moves pawn state)))))
    (t/testing "first move, leap blocked"
      (t/is (= #{[1, 6]} (let [pawn  {:piece :pawn :pos [1, 7] :color :white}
                               obstacle {:piece :pawn :pos [1, 5] :color :white}
                               state {:white [pawn obstacle] :black []}]
                            (m/moves pawn state)))))
    (t/testing "first move, full blocked"
      (t/is (= #{} (let [pawn  {:piece :pawn :pos [1, 7] :color :white}
                         obstacle {:piece :pawn :pos [1, 6] :color :white}
                         state {:white [pawn obstacle] :black []}]
                      (m/moves pawn state)))))
    (t/testing "at the edge"
      (t/is (= #{} (let [pawn  {:piece :pawn :pos [1, 1] :color :white}
                         state {:white [pawn] :black []}]
                      (m/moves pawn state))))))

  (t/testing "black pawns"
    (t/testing "first move, no obstacles"
      (t/is (= #{[1, 3] [1, 4]} (let [pawn  {:piece :pawn :pos [1, 2] :color :black}
                                      state {:white [] :black [pawn]}]
                                   (m/moves pawn state)))))
    (t/testing "first move, leap blocked"
      (t/is (= #{[1, 3]} (let [pawn  {:piece :pawn :pos [1, 2] :color :black}
                               obstacle {:piece :pawn :pos [1, 4] :color :black}
                               state {:white [] :black [pawn obstacle]}]
                            (m/moves pawn state)))))
    (t/testing "first move, full blocked"
      (t/is (= #{} (let [pawn  {:piece :pawn :pos [1, 2] :color :black}
                         obstacle {:piece :pawn :pos [1, 3] :color :black}
                         state {:white [] :black [pawn obstacle]}]
                      (m/moves pawn state)))))
    (t/testing "at the edge"
      (t/is (= #{} (let [pawn  {:piece :pawn :pos [1, 8] :color :black}
                         state {:white [] :black [pawn]}]
                      (m/moves pawn state)))))))

(t/run-tests)

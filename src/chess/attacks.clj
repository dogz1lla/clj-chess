(ns chess.attacks
  (:require [clojure.set :as s]))
     

(defn attack!
  [attacker-pos target-pos {:keys [board] :as state}]
  (let [white (:white board)
        black (:black board)
        all-pieces (vals (into black white))
        attacker-piece (->> all-pieces
                            (filter (fn [{:keys [pos]}] (= pos attacker-pos)))
                            (first))
        _ (assert attacker-piece (str "No piece found at position " attacker-pos " (attacker)"))
        attacker-color (:color attacker-piece)
        target-piece   (->> all-pieces
                            (filter (fn [{:keys [pos]}] (= pos target-pos)))
                            (first))
        ; NOTE: there is no assert for the target-piece being in the target-pos because of the
        ; en-passant
        target-piece (if target-piece
                       ; no en-passant
                       target-piece
                       ; en-passant -> we need to look for the target pawn in a neighboring cell
                       (let [[tx ty] target-pos
                             actual-pawn-pos (case attacker-color
                                               :white [tx (inc ty)]
                                               :black [tx (dec ty)])]
                         (->> all-pieces
                              (filter (fn [{:keys [pos]}] (= actual-pawn-pos pos)))
                              (first))))
        target-color   (:color target-piece)
        _ (assert (not= attacker-color target-color) "Can't attack your ally!")
        attacker-id (:id attacker-piece)
        target-id   (:id target-piece)
        attacker-allies (case attacker-color :white white :black black)
        target-allies   (case target-color :white white :black black)
        moved-attacker-piece (assoc attacker-piece :pos target-pos)

        updated-attacker-allies (assoc attacker-allies attacker-id moved-attacker-piece)
        updated-target-allies   (dissoc target-allies target-id)  ; remove the target

        new-board (-> board
                      (assoc attacker-color updated-attacker-allies)
                      (assoc target-color   updated-target-allies))
        move-history-entry {:type :attack
                            :piece-id attacker-id
                            :color attacker-color
                            :start attacker-pos
                            :finish target-pos}]
    (-> state
        (assoc :board new-board)
        (assoc :last-move move-history-entry)
        (assoc-in [:captured target-color target-id] target-piece))))
               
     


(defmulti attacks :piece)

(defmethod attacks :pawn [{:keys [pos color]} {:keys [board history last-move] :as state}]
  (let [[x y] pos
        attack-squares (case color
                          :white (if (> y 1)
                                   (case x
                                     1 #{[(inc x) (dec y)]}
                                     8 #{[(dec x) (dec y)]}
                                     #{[(inc x) (dec y)] [(dec x) (dec y)]})
                                   #{})
                          :black (if (< y 8)
                                   (case x
                                     1 #{[(inc x) (inc y)]}
                                     8 #{[(dec x) (inc y)]}
                                     #{[(inc x) (inc y)] [(dec x) (inc y)]})
                                   #{}))

        white (:white board)
        black (:black board)
        all-pieces (map second (into black white))
        opponent-pieces (case color :white black :black white)
        potential-attacks (->> opponent-pieces
                               (map second)
                               (map :pos)
                               (filter (fn [p] (attack-squares p)))
                               set)
        en-passante (if last-move
                      (let [last-moved-piece-id (:piece-id last-move)
                            last-moved-piece-color (:color last-move)
                            last-moved-piece (get (last-moved-piece-color board) last-moved-piece-id)
                            [x1 y1] (:start  last-move)
                            [x2 y2] (:finish last-move)
                            _ (assert (and x1 x2 y1 y2 last-moved-piece last-moved-piece-color))]
                       (if (and
                             #_(not= last-moved-piece-color color)  ; should always be true
                             (= :pawn (:piece last-moved-piece))
                             (or (and
                                   (= color :white)
                                   (= y1 2)
                                   (= y2 4)
                                   (= y  4))
                                 (and
                                   (= color :black)
                                   (= y1 7)
                                   (= y2 5)
                                   (= y  5)))
                             (= 1 (clojure.core/abs (- x x2))))
                         #{[x2 (case color  :white (dec y2) :black (inc y2))]}
                         #{}))
                     #{})]
    (s/union potential-attacks en-passante)))

(defmethod attacks :king [{:keys [pos color] :as piece} {:keys [board] :as state}]
  (let [[x y] pos
        dxdy (for [dx [-1 0 1]
                   dy [-1 0 1]
                   :when (or (not= 0 dx) (not= 0 dy))] [dx dy])
        all-moves (mapv (fn [[dx dy]] [(+ x dx) (+ dy y)]) dxdy)
        all-moves (set (filterv (fn [[xx yy]] (and (pos? xx) (pos? yy))) all-moves))
        white (:white board)
        black (:black board)
        opponent-pieces (case color :white black :black white)
        opponent-occupied-squares (->> opponent-pieces
                                       (map second)
                                       (map :pos)
                                       set)]
    (s/intersection all-moves opponent-occupied-squares)))

(defmethod attacks :rook [{:keys [pos color] :as piece} {:keys [board] :as state}]
   (let [[col row] pos
         ; opponent-color (case color :white :black :black :white)
         ; all-pieces (filter second board)
         ; occupied-squares (->> all-pieces (map first) set)

         white (:white board)
         black (:black board)
         all-pieces (map second (into black white))
         occupied-squares (set (map :pos all-pieces))
         opponent-pieces (map second (case color :white black :black white))

         ; horizontal line
         same-row-obstacles (filter #(= row (second %)) occupied-squares)
         same-row-obstacles-xs (map first same-row-obstacles)
         to-the-west (filter #(< % col) same-row-obstacles-xs)
         to-the-east (filter #(< col %) same-row-obstacles-xs)
         x-min (if (seq to-the-west) (apply max to-the-west) 0)
         x-max (if (seq to-the-east) (apply min to-the-east) 9)
         ; vertical line
         same-col-obstacles (filter #(= col (first %)) occupied-squares)
         same-col-obstacles-ys (map second same-col-obstacles)
         to-the-north (filter #(< % row) same-col-obstacles-ys)
         to-the-south (filter #(< row %) same-col-obstacles-ys)
         y-min (if (seq to-the-north) (apply max to-the-north) 0)
         y-max (if (seq to-the-south) (apply min to-the-south) 9)
         ; filter the pieces
         limiting-squares (set [[x-min row] [x-max row] [col y-min] [col y-max]])
         opponents-in-the-way (filter (fn [p] (limiting-squares (:pos p))) opponent-pieces)]
      (set (map :pos opponents-in-the-way))))


(defmethod attacks :bishop [{:keys [pos color] :as piece} {:keys [board] :as state}]
  (let [[col row] pos
        white (:white board)
        black (:black board)
        all-pieces (map second (into black white))
        occupied-squares (set (map :pos all-pieces))
        opponent-pieces (map second (case color :white black :black white))
        opponent-squares (set (map :pos opponent-pieces))
        rays [(map (fn [c] [(* c +1) (* c +1)]) (range 1 9))
              (map (fn [c] [(* c +1) (* c -1)]) (range 1 9))
              (map (fn [c] [(* c -1) (* c +1)]) (range 1 9))
              (map (fn [c] [(* c -1) (* c -1)]) (range 1 9))]
        center-on-piece (fn [ray] (map (fn [[x y]] [(+ x col) (+ y row)]) ray))
        filter-out-of-bounds (fn [ray] (filter (fn [[x y]] (and (pos? x) (pos? y))) ray))
        filter-attack-rays (fn [ray] (first (filter #(occupied-squares %) ray)))
        rays (->> rays
                  (map center-on-piece)
                  (map filter-out-of-bounds)
                  (map filter-attack-rays)
                  (remove nil?)
                  (filter #(opponent-squares %)))]
    (set rays)))


(defmethod attacks :knight [{:keys [pos color] :as piece} {:keys [board] :as state}]
  (let [[col row] pos
        white (:white board)
        black (:black board)
        all-pieces (map second (into black white))
        occupied-squares (set (map :pos all-pieces))
        opponent-pieces (map second (case color :white black :black white))
        opponent-squares (set (map :pos opponent-pieces))
        ds (for [i [-1 1 -2 2] j [-1 1 -2 2] :when (not= (abs i) (abs j))] [i j])
        moves (->> ds
                   (map (fn [[dx dy]] [(+ col dx) (+ row dy)]))
                   (filter (fn [[x y]] (and (pos? x) (pos? y) (< x 9) (< y 9))))
                   (filter (fn [square] (opponent-squares square))))]
    (set moves)))


(defmethod attacks :queen [{:keys [pos color] :as piece} {:keys [board] :as state}]
  (let [[col row] pos
        ; opponent-color (case color :white :black :black :white)
        ; occupied-squares (->> board
        ;                       (filter second)
        ;                       ; (filter #(not= color (:color (second %))))
        ;                       (map first)
        ;                       set)
        white (:white board)
        black (:black board)
        all-pieces (map second (into black white))
        occupied-squares (set (map :pos all-pieces))
        opponent-pieces (map second (case color :white black :black white))
        opponent-squares (set (map :pos opponent-pieces))

        rays  [(for [c (range 1 9) :let [ci (* c -1) cj (* c -1)]]  [(+ col ci) (+ row cj)])
               (for [c (range 1 9) :let [ci (* c +1) cj (* c -1)]]  [(+ col ci) (+ row cj)])
               (for [c (range 1 9) :let [ci (* c -1) cj (* c +1)]]  [(+ col ci) (+ row cj)])
               (for [c (range 1 9) :let [ci (* c +1) cj (* c +1)]]  [(+ col ci) (+ row cj)])]
        cross [(for [i (range (inc col) 9)]    [i row])
               (for [i (range (dec col) 0 -1)] [i row])
               (for [i (range (inc row) 9)]    [col i])
               (for [i (range (dec row) 0 -1)] [col i])]

        filter-out-of-bounds (fn [ray] (filter #(and (pos? (first %)) (pos? (second %))) ray))
        filter-attack-rays (fn [ray] (first (filter #(occupied-squares %) ray)))
        filter-opponent-pieces #(opponent-squares %)

        rays (->> rays
                  (map filter-out-of-bounds)
                  (map filter-attack-rays)
                  (remove nil?)
                  (filter filter-opponent-pieces))
        cross (->> cross
                   (map filter-out-of-bounds)
                   (map filter-attack-rays)
                   (remove nil?)
                   (filter filter-opponent-pieces))]
    (set (vec (concat rays cross)))))


(comment 
  (remove nil? [nil])
  (some #(when (> % 10) %) [1 5 1 8 1])
  (first (filter #(> % 10) [1 5 1 8 1]))
  (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
        other   {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 2]}
        state   {:board {[3 3] pawn [2 2] other [4 2] another} :history [pawn]}]
    (attacks pawn state))
  (let [bishop    {:piece :bishop :color :white :pos [3, 3]}
        other   {:piece :pawn :color :black :pos [4, 4]}
        another {:piece :pawn :color :black :pos [5, 5]}
        state   {:board {[3 3] bishop [4 4] other [5 5] another}}]
    (attacks bishop state))
  (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
        target  {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 2]}
        state   {:board {[3 3] pawn [2 2] target [4 2] another} :history [pawn] :captured []}
        attack-square [2 2]]
    (attack! attack-square pawn state))
  (let [queen   {:piece :queen :color :white :pos [3, 3]}
        other   {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 3]}
        state   {:board {[3 3] queen [2 2] other [4 3] another}}]
    (attacks queen state)))

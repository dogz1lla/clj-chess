(ns chess.moves
  "Calculate possible move squares for each piece type. The move squares depend on the position of
  the piece in question and the positions of all the other pieces.
  Important: moves are different from attacks!"
  (:require [clojure.set :as s]
            [chess.state :as state]))


(defn move!
  "Find the piece by the having (:pos piece) = start, then switch :pos to finish.
  TODO: add an assert that piece is not nil"
  [start finish {:keys [board] :as state}
    (let [white (:white board)
          black (:black board)
          all-pieces (into black white)
          piece (first (filter (fn [[_ {:keys [pos]}]] (= pos start)) all-pieces)) 
          piece-id (:id piece)
          piece-color (:color piece)]
      (assoc-in state [:board piece-color piece-id :pos] finish))])

(defmulti moves :piece)


(defmethod moves :pawn [{:keys [pos color]} {:keys [board]}]
  (let [[x y] pos
        step (case color
               :white (max 1 (dec y))
               :black (min 8 (inc y)))
        leap (case color
               :white (max 1 (dec step))
               :black (min 8 (inc step)))
        ideal-moves (case color
                      :white (if (> y 1) #{[x step]} #{})
                      :black (if (< y 8) #{[x step]} #{}))
        leap-move   (case color
                      :white (if (= y 7) #{[x leap]} #{})
                      :black (if (= y 2) #{[x leap]} #{}))
        ; occupied-squares (filter second board)  ; when the value for the key is non-nil
        white (:white board)
        black (:black board)
        all-pieces (map second (into black white))
        blocked? (some identity  ; why not or? see https://stackoverflow.com/a/2969551
                   (map
                     (fn [{:keys [pos]}]
                       (let [[x-pos y-pos] pos]
                         (and (= x-pos x) (= y-pos step))))
                     all-pieces))
        leap-blocked? (some identity  ; why not or? see https://stackoverflow.com/a/2969551
                        (map
                          (fn [{:keys [pos]}]
                            (let [[x-pos y-pos] pos]
                              (and (= x-pos x) (= y-pos leap))))
                          all-pieces))
        result (if blocked?
                 #{}
                 (if leap-blocked?
                   ideal-moves
                   (s/union ideal-moves leap-move)))]
    result))

(defmethod moves :king [{:keys [pos color]} {:keys [board]}]
  (let [[x y] pos
        dxdy (for [dx [-1 0 1]
                   dy [-1 0 1]
                   :when (or (not= 0 dx) (not= 0 dy))] [dx dy])
        ideal-moves (mapv (fn [[dx dy]] [(+ x dx) (+ dy y)]) dxdy)
        ideal-moves (filterv (fn [[xx yy]] (and (pos? xx) (pos? yy))) ideal-moves)
        ideal-moves (filter (fn [[x y]] (and (pos? x) (pos? y))) ideal-moves)
        ideal-moves (filter (fn [[x y]] (and (< x 9) (< y 9))) ideal-moves)
        ideal-moves (set ideal-moves)
        occupied-squares (set (map first (filter second board)))]  ; when the value for the key is non-nil
    (s/difference ideal-moves occupied-squares)))

(defmethod moves :rook [{:keys [pos color]} {:keys [board]}]
  (let [[col row] pos
        ; opponent-color (case color :white :black :black :white)
        occupied-squares (->> board
                              (filter second)
                              (map first)
                              set)
        ; horizontal line
        horizontal-moves (for [x (range 1 9) :when (not= x col)] [x row])
        same-row-obstacles (filter #(= row (second %)) occupied-squares)
        same-row-obstacles-xs (map first same-row-obstacles)
        to-the-west (filter #(< % col) same-row-obstacles-xs)
        to-the-east (filter #(< col %) same-row-obstacles-xs)
        x-min (if (seq to-the-west) (apply max to-the-west) 0)
        x-max (if (seq to-the-east) (apply min to-the-east) 9)
        ; vertical line
        vertical-moves (for [y (range 1 9) :when (not= y row)] [col y])
        same-col-obstacles (filter #(= col (first %)) occupied-squares)
        same-col-obstacles-ys (map second same-col-obstacles)
        to-the-north (filter #(< % row) same-col-obstacles-ys)
        to-the-south (filter #(< row %) same-col-obstacles-ys)
        y-min (if (seq to-the-north) (apply max to-the-north) 0)
        y-max (if (seq to-the-south) (apply min to-the-south) 9)
        ; filter the moves
        horizontal-moves (set (filter (fn [[x y]] (and (< x-min x) (< x x-max))) horizontal-moves))
        vertical-moves (set (filter (fn [[x y]] (and (< y-min y) (< y y-max))) vertical-moves))]
    (s/union horizontal-moves vertical-moves)))
    

(defmethod moves :bishop [{:keys [pos color]} {:keys [board]}]
  (let [[col row] pos
        ; opponent-color (case color :white :black :black :white)
        occupied-squares (->> board
                              (filter second)
                              (map first)
                              set)

        ; rays (for [i [-1 1] j [-1 1]] [i j])
        rays [(map (fn [c] [(* c +1) (* c +1)]) (range 1 9))
              (map (fn [c] [(* c +1) (* c -1)]) (range 1 9))
              (map (fn [c] [(* c -1) (* c +1)]) (range 1 9))
              (map (fn [c] [(* c -1) (* c -1)]) (range 1 9))]
        center-on-piece (fn [ray] (map (fn [[x y]] [(+ x col) (+ y row)]) ray))
        filter-out-of-bounds (fn [ray] (filter (fn [[x y]] (and (pos? x) (pos? y))) ray))
        filter-move-rays (fn [ray] (take-while #(not (occupied-squares %)) ray))
        rays (map center-on-piece rays)
        rays (map filter-out-of-bounds rays)
        rays (map filter-move-rays rays)]
    (set (reduce into [] rays))))
              

(defmethod moves :knight [{:keys [pos color]} {:keys [board]}]
  (let [[col row] pos
        occupied-squares (->> board
                              (filter second)
                              (map first)
                              set)
        ds (for [i [-1 1 -2 2] j [-1 1 -2 2] :when (not= (abs i) (abs j))] [i j])
        moves (->> ds
                   (map (fn [[dx dy]] [(+ col dx) (+ row dy)]))
                   (filter (fn [[x y]] (and (pos? x) (pos? y) (< x 9) (< y 9))))
                   (filter (fn [square] (not (occupied-squares square)))))]
    (set moves)))
              

(defmethod moves :queen [{:keys [pos color]} {:keys [board]}]
  (let [[col row] pos
        occupied-squares (->> board
                              (filter second)
                              (map first)
                              set)
        rays  [(for [c (range 1 9) :let [ci (* c -1) cj (* c -1)]]  [(+ col ci) (+ row cj)])
               (for [c (range 1 9) :let [ci (* c +1) cj (* c -1)]]  [(+ col ci) (+ row cj)])
               (for [c (range 1 9) :let [ci (* c -1) cj (* c +1)]]  [(+ col ci) (+ row cj)])
               (for [c (range 1 9) :let [ci (* c +1) cj (* c +1)]]  [(+ col ci) (+ row cj)])]
        cross [(for [i (range (inc col) 9)]    [i row])
               (for [i (range (dec col) 0 -1)] [i row])
               (for [i (range (inc row) 9)]    [col i])
               (for [i (range (dec row) 0 -1)] [col i])]

        filter-out-of-bounds (fn [ray] (filter #(and (pos? (first %)) (pos? (second %))) ray))
        filter-till-piece    (fn [ray] (take-while (fn [cell] (not (occupied-squares cell))) ray))

        rays  (map filter-out-of-bounds rays)
        rays  (map filter-till-piece rays)
        cross (map filter-out-of-bounds cross)
        cross (map filter-till-piece cross)]
    (set (reduce into [] (concat rays cross)))))
        

(comment
  (let [state (state/init-state)
        pos [1 2]
        piece (get (:board state) pos)]
    (moves piece state))
  (let [state (state/init-state)
        pos [5 1]
        piece (get (:board state) pos)]
      (moves piece state))
  (let [start [1 2]
        finish [1 4]
        state (state/init-state)]
     (move! start finish state))
  (let [state (state/init-state)
        pos [3 8]
        piece (get (:board state) pos)]
      (moves piece state))
  (let [state (state/init-state)
        pos [3 8]
        piece (get (:board state) pos)]
      (moves piece state))
  (let [piece {:piece :queen :pos [5 4] :color :white}
        state {:board (state/put-piece-on-board (state/init-board) piece)}]
    (moves piece state))
  
  (for [i [-1 1 -2 2] j [-1 1 -2 2] :when (not= (abs i) (abs j))] [i j])
  (map (fn [c] [(* c 1) (* c 1)]) (range 1 9))
  (filter (fn [[x y]] (and (pos? x) (pos? y))) (map (fn [c] [(* c 1) (* c 1)]) (range 1 9)))
  (take-while #(not (#{[5 5] [2 2]} %)) (map (fn [c] [(* c 1) (* c 1)]) (range 1 9)))
  (for [i (range 1 9) j (range 1 9) :when (and (or (= 4 i) (= 5 j)) (not= [i j] [4 5]))] [i j])
  (for [i [-1 1] j [-1 1] c (range 1 9) :let [ci (* c i) cj (* c j)]]  [ci cj])
  (map (fn [c] (let [x (for [i [-1 1] j [-1 1]] [i j])] (map #()))))
  (into {:1 1} {:2 2 :3 2}))

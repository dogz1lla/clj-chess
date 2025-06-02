(ns chess.moves
  "Calculate possible move squares for each piece type. The move squares depend on the position of
  the piece in question and the positions of all the other pieces.
  Important: moves are different from attacks!"
  (:require [clojure.set :as s]
            [chess.state :as state]))


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
        occupied-squares (filter second board)  ; when the value for the key is non-nil
        all-pieces (map second occupied-squares)
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

(defn move! [start finish {:keys [board history] :as state}]
  (let [piece (get board start)
        moved-piece (assoc piece :pos finish)
        new-board (-> board
                      (assoc start nil)
                      (assoc finish moved-piece))]
     (-> state
         (assoc :board new-board)
         (assoc :history (conj history piece)))))
    

(comment
  (let [state (state/init-state)
        pos [1 2]
        piece (get (:board state) pos)]
    (moves piece state))
  (let [start [1 2]
        finish [1 4]
        state (state/init-state)]
    (move! start finish state)))

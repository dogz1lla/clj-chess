(ns chess.moves
  "Calculate possible move squares for each piece type. The move squares depend on the position of
  the piece in question and the positions of all the other pieces.
  Important: moves are different from attacks!"
  (:require [clojure.set :as s]))


(defmulti moves :piece)

(defmethod moves :pawn [{:keys [pos color]} {:keys [white black]}]
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
        all-pieces (concat white black)
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


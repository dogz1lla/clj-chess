(ns chess.attacks
  (:require [clojure.set :as s]))


(defn attack!
  "TODO: rewrite the history logic (include start and finish squares too)"
  [attack-square {:keys [pos color] :as attacking-piece} {:keys [board history captured] :as state}]
  (let [occupied-squares (filter second board)  ; when the value for the key is non-nil
        all-pieces (map second occupied-squares)
        white (filter #(= :white (:color %)) all-pieces)
        black (filter #(= :black (:color %)) all-pieces)
        ally-pieces (case color :white white :black black)
        opponent-color (case color :white :black :black :white)
        opponent-pieces (case opponent-color :white white :black black)
        target (->> opponent-pieces (filter (fn [{:keys [pos]}] (= attack-square pos))) first)
        remaining-opponent-pieces (filterv (fn [p] (not (= target p))) opponent-pieces)
        remaining-ally-pieces (filterv (fn [p] (not (= attacking-piece p))) ally-pieces)
        moved-attacking-piece (assoc attacking-piece :pos attack-square)
        new-board (-> board
                      (assoc attack-square moved-attacking-piece)
                      (assoc pos nil))]
    (println occupied-squares all-pieces)
    (if target
      (-> state
        (assoc :board new-board)
        (assoc :captured (conj captured target))
        (assoc :history (conj history attacking-piece)))
      state)))
     


(defmulti attacks :piece)

(defmethod attacks :pawn [{:keys [pos color]} {:keys [board history] :as state}]
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
        occupied-squares (filter second board)  ; when the value for the key is non-nil
        all-pieces (map second occupied-squares)
        white (filter #(= :white (:color %)) all-pieces)
        black (filter #(= :black (:color %)) all-pieces)
        opponent-pieces (case color :white black :black white)
        potential-attacks (->> opponent-pieces
                               (map :pos)
                               (filter (fn [p] (attack-squares p)))
                               set)
        ; TODO: add en-passante
        en-passante #{} #_(let [last-moved-piece (last history)]  ; en-passante
                            (if (and
                                  ; NOTE: this assumes that there is no piece behind the pawn in question
                                  ; but if everything is coded correctly it cant be the case because then
                                  ; the pawn couldnt have moved two squares up
                                  (= :pawn (:piece last-moved-piece))
                                  (or
                                    (and
                                      ; white pawn on 4th rank moved last and we are playing as black pawn
                                      (= :white (:color last-moved-piece))
                                      (= :black color)
                                      (= 4 (first (:pos last-moved-piece)) y))
                                    (and
                                      ; black pawn on 5th rank moved last and we are playing as white pawn
                                      (= :black (:color last-moved-piece))
                                      (= :white color)
                                      (= 5 (first (:pos last-moved-piece)) y)))
                                  (:leap last-moved-piece)  ; make sure that it jumped instead of 2x1 moves
                                  (= 1 (clojure.core/abs (- x (second (:pos last-moved-piece))))))
                              [(assoc last-moved-piece :enpassante true)]
                              []))]
    (s/union potential-attacks en-passante)))

(defmethod attacks :king [{:keys [pos color] :as piece} {:keys [board] :as state}]
  (let [[x y] pos
        dxdy (for [dx [-1 0 1]
                   dy [-1 0 1]
                   :when (or (not= 0 dx) (not= 0 dy))] [dx dy])
        all-moves (mapv (fn [[dx dy]] [(+ x dx) (+ dy y)]) dxdy)
        all-moves (set (filterv (fn [[xx yy]] (and (pos? xx) (pos? yy))) all-moves))
        opponent-color (case color :white :black :black :white)
        opponent-occupied-squares (->> board
                                       (filter second)
                                       (filter (fn [[_ p]] (= (:color p) opponent-color)))
                                       (map first)
                                       set)]
    (println opponent-occupied-squares)
    (s/intersection all-moves opponent-occupied-squares)))

(defmethod attacks :rook [{:keys [pos color] :as piece} {:keys [board] :as state}]
   (let [[col row] pos
         opponent-color (case color :white :black :black :white)
         all-pieces (filter second board)
         occupied-squares (->> all-pieces (map first) set)
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
         opponents-in-the-way (filter (fn [p] (and (= opponent-color (:color p)) (limiting-squares (:pos p)))) (map second all-pieces))]
      (set (map :pos opponents-in-the-way))))


(comment 
  (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
        other   {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 2]}
        state   {:board {[3 3] pawn [2 2] other [4 2] another} :history [pawn]}]
    (attacks pawn state))
  (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
        target  {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 2]}
        state   {:board {[3 3] pawn [2 2] target [4 2] another} :history [pawn] :captured []}
        attack-square [2 2]]
    (attack! attack-square pawn state)))

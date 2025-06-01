(ns chess.attacks
  (:require [clojure.set :as s]))


(defmulti attacks :piece)

(defmethod attacks :pawn [{:keys [pos color]} {:keys [white black move-history]}]
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
        opponent-pieces (case color :white black :black white)
        potential-attacks (->> opponent-pieces
                               (map :pos)
                               (filter (fn [p] (attack-squares p)))
                               set)
        ; TODO: add en-passante
        en-passante #{} #_(let [last-moved-piece (last move-history)]  ; en-passante
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
    (println attack-squares)
    (s/union potential-attacks en-passante)))


(defn attack!
  "TODO: rewrite after the state shape is reconsidered"
  [attack-square {:keys [pos color] :as attacking-piece} {:keys [white black move-history captured-pieces] :as state}]
  (let [ally-pieces (case color :white white :black black)
        opponent-color (case color :white :black :black :white)
        opponent-pieces (case opponent-color :white white :black black)
        target (->> opponent-pieces (filter (fn [{:keys [pos]}] (= attack-square pos))) first)
        remaining-opponent-pieces (filterv (fn [p] (not (= target p))) opponent-pieces)
        remaining-ally-pieces (filterv (fn [p] (not (= attacking-piece p))) ally-pieces)
        moved-attacking-piece (assoc attacking-piece :pos attack-square)]
    (when target
      (-> state
        (assoc color (conj remaining-ally-pieces moved-attacking-piece))
        (assoc :captured-pieces (conj captured-pieces target))
        (assoc opponent-color remaining-opponent-pieces)
        (assoc :move-history (conj move-history attacking-piece))))))
     

(comment 
  (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
        other   {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 2]}
        state   {:white [pawn] :black [other another] :move-history [pawn]}]
    (attacks pawn state))
  (let [pawn    {:piece :pawn :color :white :pos [3, 3]}
        other   {:piece :pawn :color :black :pos [2, 2]}
        another {:piece :pawn :color :black :pos [4, 2]}
        state   {:white [pawn] :black [other another] :move-history [pawn]}
        attack-square [2 2]]
    (attack! attack-square pawn state)))

(ns chess-clj.infra
  (:require  [clojure.contrib.math :refer :all]
             [clojure.contrib.string :refer :all]
             [clojure.contrib.combinatorics :refer :all]))

(def movements (ref []))

(def chessboard
  "Initial board using algebric coordinates"
  (ref
   {:a8 "" :b8 "" :c8 "" :d8 "" :e8 "" :f8 "" :g8 "" :h8 ""
    :a7 "" :b7 "" :c7 "" :d7 "" :e7 "" :f7 "" :g7 "" :h7 ""
    :a6 "" :b6 "" :c6 "" :d6 "" :e6 "" :f6 "" :g6 "" :h6 ""
    :a5 "" :b5 "" :c5 "" :d5 "" :e5 "" :f5 "" :g5 "" :h5 ""
    :a4 "" :b4 "" :c4 "" :d4 "" :e4 "" :f4 "" :g4 "" :h4 ""
    :a3 "" :b3 "" :c3 "" :d3 "" :e3 "" :f3 "" :g3 "" :h3 ""
    :a2 "" :b2 "" :c2 "" :d2 "" :e2 "" :f2 "" :g2 "" :h2 ""
    :a1 "" :b1 "" :c1 "" :d1 "" :e1 "" :f1 "" :g1 "" :h1 ""}))

(defn init-chessboard []
  "Distribute pieces into chessboard"
  (dosync
   (alter chessboard assoc :a8 "r" :b8 "n" :c8 "b" :d8 "q" :e8 "k" :f8 "b" :g8 "n" :h8 "r"
                           :a7 "p" :b7 "p" :c7 "p" :d7 "p" :e7 "p" :f7 "p" :g7 "p" :h7 "p"
                           :a6 "" :b6 "" :c6 "" :d6 "" :e6 "" :f6 "" :g6 "" :h6 ""
                           :a5 "" :b5 "" :c5 "" :d5 "" :e5 "" :f5 "" :g5 "" :h5 ""
                           :a4 "" :b4 "" :c4 "" :d4 "" :e4 "" :f4 "" :g4 "" :h4 ""
                           :a3 "" :b3 "" :c3 "" :d3 "" :e3 "" :f3 "" :g3 "" :h3 ""
                           :a2 "P" :b2 "P" :c2 "P" :d2 "P" :e2 "P" :f2 "P" :g2 "P" :h2 "P"
                           :a1 "R" :b1 "N" :c1 "B" :d1 "Q" :e1 "K" :f1 "B" :g1 "N" :h1 "R")))

(defn coordinate? [coordinate]
  (contains? @chessboard (keyword coordinate)))

(defn piece-at [coordinate]
  (@chessboard (keyword coordinate)))

(defn make-movement [collection]
  (let [piece (piece-at (first collection))]
    (dosync (alter chessboard assoc (keyword (first collection)) "" (keyword (second collection)) piece))))

(defn line [coordinate]
  (read-string (subs coordinate 1 2)))

(defn lazy-contains? [col piece]
 (some #(= piece %) col))1

(defn inc-col [col offset]
  (let [carac (+ offset (apply int (seq (lower-case col))))]
    (cond
       (< carac 97) ""
       (> carac 104) ""
       :else (str (char carac)))))

(defn horiz [coordinate offset]
  (let [col (subs coordinate 0 1)
        line (subs coordinate 1 2)]
    (str (inc-col col offset) line)))

(defn vert [coordinate offset]
  (let [col (subs coordinate 0 1)
        line (read-string (subs coordinate 1 2))]
    (str col (+ line offset))))

(defn ldiag [coordinate offset]
  (let [nc (* -1 (abs offset))]
    (horiz (vert coordinate offset) nc)))

(defn rdiag [coordinate offset]
  (let [nc (abs offset)]
    (horiz (vert coordinate offset) nc)))

(defn combination []
  (filter (fn [x]
     (not= (abs (first x)) (abs (second x)))) (combinations [-2 -1 1 2] 2)))

(defn knight-combination []
    (loop [c (combination) acum '()]
      (if (empty? c)
        acum
        (let [l (permutations (first c))
              a (first l)
              b (second l)]
        (recur (rest c) (conj acum a b ))))))

(defn get-key [coordinate]
  (-> coordinate piece-at lower-case))

(defn u-case? [x]
  (= x (upper-case x)))

(defn same-color-piece? [piece coordinate]
  (= (u-case? piece) (-> coordinate piece-at u-case?)))

(defn parse-move [movement]
   (let [parsed-movement (re-matches #"[a-h][1-8]-[a-h][1-8]$" movement)]
     (if (not (empty? parsed-movement))
       (conj '() (subs parsed-movement 3 5) (subs parsed-movement 0 2)))))

(defn add-moves [movement]
  "Add moves into movements list - To make a move its only necessary to specify origin coordinate and destiny"
  (dosync (alter movements conj movement)))

(defn reset-moves []
  "Reset all moves"
  (dosync (ref-set movements [])))

(defn turn []
  (if (odd? (count @movements))
    "black"
    "white"))

(defn white? [piece]
  (and (= piece (upper-case piece)) (not (blank? piece))))

(defn black? [piece]
  (and (= piece (lower-case piece)) (not (blank? piece))))

(defn enemy-piece? [coordinate]
  (let [piece (piece-at coordinate)]
    (or
       (and (= (turn) "white") (black? piece))
       (and (= (turn) "black") (white? piece)))))

(defn apply-move-func [func coordinate offset]
  (loop [i offset acum '()]
    (let [c (func coordinate i)]
      (if (or (not (empty? (piece-at c))) (not (coordinate? c)))
        (if (and (coordinate? c) (enemy-piece? c))
          (cons c acum)
          acum)
        (if (< i 0)
          (recur (dec i) (cons c acum))
          (recur (inc i) (cons c acum)))))))

(defn first-pawn-move? [coordinate]
  (let [piece (piece-at coordinate)]
    (or (and (white? piece) (= 2 (line coordinate)))
        (and (black? piece) (= 7 (line coordinate))))))

(defn valid-turn? [coordinate]
  (or
   (and (= (turn) "white") (white? (piece-at coordinate)))
   (and (= (turn) "black") (black? (piece-at coordinate)))))


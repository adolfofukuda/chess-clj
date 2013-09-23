(ns chess-clj.infra
  (:require  [clojure.contrib.math :refer :all]
             [clojure.contrib.string :refer :all]
             [clojure.contrib.combinatorics :refer :all]))
(defrecord Chessboard
  [a8 b8 c8 d8 e8 f8 g8 h8
   a7 b7 c7 d7 e7 f7 g7 h7
   a6 b6 c6 d6 e6 f6 g6 h6
   a5 b5 c5 d5 e5 f5 g5 h5
   a4 b4 c4 d4 e4 f4 g4 h4
   a3 b3 c3 d3 e3 f3 g3 h3
   a2 b2 c2 d2 e2 f2 g2 h2
   a1 b1 c1 d1 e1 f1 g1 h1])

(defn init-chessboard []
   (->Chessboard "r" "n" "b" "q" "k" "b" "n" "r"
                 "p" "p" "p" "p" "p" "p" "p" "p"
                 "" "" "" "" "" "" "" ""
                 "" "" "" "" "" "" "" ""
                 "" "" "" "" "" "" "" ""
                 "" "" "" "" "" "" "" ""
                 "P" "P" "P" "P" "P" "P" "P" "P"
                 "R" "N" "B" "Q" "K" "B" "N" "R"))

(defn coordinate? [coordinate]
  (not (nil? (re-matches #"[a-h][1-8]$" coordinate))))

(defn piece-at [chessboard coordinate]
  ((keyword coordinate) chessboard))

(defn make-movement [chessboard collection]
  (let [piece (piece-at chessboard (first collection))]
    (assoc chessboard (keyword (first collection)) "" (keyword (second collection)) piece)))

(defn line [coordinate]
  (read-string (subs coordinate 1 2)))

(defn lazy-contains? [col piece]
 (some #(= piece %) col))

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

(defn get-key [chessboard coordinate]
  (lower-case (piece-at chessboard coordinate)))

(defn u-case? [x]
  (= x (upper-case x)))

(defn same-color-piece? [chessboard piece coordinate]
  (= (u-case? piece) (u-case? (piece-at chessboard coordinate))))

(defn parse-move [movement]
   (let [parsed-movement (re-matches #"[a-h][1-8]-[a-h][1-8]$" movement)]
     (if (not (empty? parsed-movement))
       (conj '() (subs parsed-movement 3 5) (subs parsed-movement 0 2)))))


(defn white? [piece]
  (and (= piece (upper-case piece)) (not (blank? piece))))

(defn black? [piece]
  (and (= piece (lower-case piece)) (not (blank? piece))))

(defn enemy-piece? [chessboard piece coordinate]
  (let [piece-at-coordinate (piece-at chessboard coordinate)]
    (or
       (and (= white? piece-at-coordinate) (black? piece))
       (and (= black? piece-at-coordinate) (white? piece)))))

(defn apply-move-func [chessboard func coordinate offset]
  (loop [i offset acum '()]
    (let [c (func coordinate i)]
      (if (or (not (empty? (piece-at chessboard c))) (not (coordinate? c)))
        (if (and (coordinate? c) (enemy-piece? chessbord (piece-at chessboard coordinate) c ))
          (cons c acum)
          acum)
        (if (< i 0)
          (recur (dec i) (cons c acum))
          (recur (inc i) (cons c acum)))))))

(defn first-pawn-move? [coordinate]
  (let [piece (piece-at coordinate)]
    (or (and (white? piece) (= 2 (line coordinate)))
        (and (black? piece) (= 7 (line coordinate))))))



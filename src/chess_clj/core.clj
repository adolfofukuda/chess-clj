(ns chess-clj.core
  (:require  [clojure.contrib.string :refer :all]
             [chess-clj.infra :refer :all]))

(defn init-game []
  (reset-moves)
  (init-chessboard))

(defmulti piece-move get-key)

(defmethod piece-move "p" [chessboard coordinate]
  (let [s   (if (white? (piece-at chessboard coordinate)) 1 -1)
        d   (if (white? (piece-at chessboard coordinate)) 2 -2)
        mov1 (conj [] (vert coordinate s))
        mov2 (conj [] (ldiag coordinate s) (rdiag coordinate s))
        mov3 (conj [] (vert coordinate d))]
     (concat (filter #(blank? (piece-at chessboard %)) mov1)
             (filter #(and (coordinate? %) (enemy-piece? chessboard %)) mov2)
             (filter #(and
                      (blank? (piece-at %)) (first-pawn-move? coordinate)) mov3))))

(defmethod piece-move "n" [coordinate]
  (filter #(and
            (or (empty? (piece-at %)) (not (same-color-piece? (piece-at %) coordinate))) (coordinate? %) )
          (map #(horiz (vert coordinate (first %)) (second %)) (knight-combination))))

(defmethod piece-move "r" [coordinate]
  (let [a (apply-move-func horiz coordinate 1)
        b (apply-move-func horiz coordinate -1)
        c (apply-move-func vert coordinate 1)
        d (apply-move-func vert coordinate -1)]
    (concat a b c d)))

(defmethod piece-move "b" [coordinate]
  (let [a (apply-move-func rdiag coordinate 1)
        b (apply-move-func rdiag coordinate -1)
        c (apply-move-func ldiag coordinate 1)
        d (apply-move-func ldiag coordinate -1)]
    (concat a b c d)))

(defmethod piece-move "q" [coordinate]
  (let [a (apply-move-func rdiag coordinate 1)
        b (apply-move-func rdiag coordinate -1)
        c (apply-move-func ldiag coordinate 1)
        d (apply-move-func ldiag coordinate -1)
        e (apply-move-func horiz coordinate 1)
        f (apply-move-func horiz coordinate -1)
        g (apply-move-func vert coordinate 1)
        h (apply-move-func vert coordinate -1)]
    (concat a b c d e f g h)))

(defn move [movement]
  (let [col (parse-move movement)
        orig-coordinate (first col)
        dest-coordinate (second col)]
      (if (and (lazy-contains? (piece-move orig-coordinate) dest-coordinate) (valid-turn? orig-coordinate))
        ((make-movement col)
        (add-moves movement))
        (throw (Exception. "NOT A LEGAL MOVEMENT")))))


(defn -main
  "The application's main function"
  [& args]
  (if args
    (println (str "You passed in this value: " args))
    (println (str "Testing..."))))





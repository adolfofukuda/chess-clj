(ns chess-clj.core-test
  (:require [clojure.test :refer :all]
            [chess-clj.core :refer :all]
            [clojure.contrib.string :refer :all]))

(defn test-setup-fixture [f]
  (init-game)
  (f))

(use-fixtures :once test-setup-fixture)

(defmulti write class)
(defmethod write String [s]
  (println s))
(defmethod write nil [s]
  (println "nil"))
(defmethod write Number [n]
  (println (str n)))
(defmethod write java.util.Collection [c]
  (print "(" )
  (print (join " " c))
  (print ")"))


(deftest init-game-test
  (testing "Testing init game"
    (is (= (piece-at "e1") "K"))))

(deftest white-knight-move
  (testing "Testing all possible position of a Knight in b1"
    (is (not (empty? (piece-move "b1"))))
    (write (piece-move "b1"))))



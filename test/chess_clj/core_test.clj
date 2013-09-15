(ns chess-clj.core-test
  (:require [clojure.test :refer :all]
            [chess-clj.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(defmulti multi-fun
  (fn [x y] (+ x y)))
(defmethod multi-fun 3 [x y]
  (print "Specialisation 3"))
(defmethod multi-fun :default [x y]
  (print "Generic " (+ x y)))

(deftest defmul
  (testing "teste" 
           (is (= 3 (multi-fun 1 2)))))
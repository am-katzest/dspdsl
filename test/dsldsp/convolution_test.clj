(ns dsldsp.convolution-test
  (:require
   [clojure.test :refer [deftest is]]
   [dsldsp.convolution :as s]))

(deftest convolute-test
  (is (= '(1.0 2.0 3.0 2.0 1.0)
         (s/convolute [1 1 1] [1 1 1])))
  (is (= [1.0 1.0 1.0]
         (s/convolute [1 1 1] [1])))
  (is (= '(0.0 0.0 4.0 0.0 0.0)
         (s/convolute [0 2 0] [0 2 0]))))

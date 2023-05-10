(ns dsldsp.convolution-test
  (:require
   [clojure.test :refer [deftest is]]
   [dsldsp.convolution :as s]))

(deftest convolute-test
  (is (= '(1.0 2.0 3.0 2.0 1.0)
         (s/convolute [1 1 1] [1 1 1])))
  (is (= [1.0 1.0 1.0]
         (s/convolute [1 1 1] [1])))
  (is (= [1.0 1.0 1.0]
         (s/convolute [1] [1 1 1])))
  (is (= [10.0 23.0 32.0 16.0]
         (s/convolute [5 4] [2 3 4])))
  (is (= [10.0 23.0 32.0 16.0]
         (s/convolute [2 3 4] [5 4])))
  (is (= '(0.0 0.0 4.0 0.0 0.0)
         (s/convolute [0 2 0] [0 2 0]))))
(deftest correlate-test
  (is (= '(1.0 2.0 3.0 2.0 1.0)
         (s/correlate [1 1 1] [1 1 1])))
  (is (= [1.0 1.0 1.0]
         (s/correlate [1 1 1] [1])))
  (is (= [1.0 1.0 1.0]
         (s/correlate [1] [1 1 1])))
  (is (= [10.0 23.0 32.0 16.0]
         (s/correlate [5 4] [4 3 2])))
  (is (= [10.0 23.0 32.0 16.0]
         (s/correlate [2 3 4] [4 5])))
  (is (= '(0.0 0.0 4.0 0.0 0.0)
         (s/correlate [0 2 0] [0 2 0]))))

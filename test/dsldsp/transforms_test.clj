(ns dsldsp.transforms-test
  (:require [dsldsp.transforms :as sut]
            [clojure.test :as t]))

(t/deftest reversing
  (t/are [b a] (= [a b] [(sut/rev-int 3 b) b])
    0 0
    1 4
    2 2
    3 6
    4 1
    5 5
    6 3
    7 7)
  (t/are [b a] (= [a b] [(sut/rev-int 4 b) b])
    0 0
    1 8
    2 4
    3 12
    4 2
    5 10
    6 6
    7 14
    8 1))

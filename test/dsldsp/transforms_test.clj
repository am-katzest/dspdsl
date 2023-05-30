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
    7 7))

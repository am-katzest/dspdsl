(ns dsldsp.transforms-test
  (:require [dsldsp.transforms :as sut]
            [dsldsp.signal :as s]
            [clojure.test :as t]
            [dsldsp.graph :as g]
            [complex.core :as c]))

(def ex-arr16 (object-array (map c/complex (range 16))))

(def ex-arr64 (object-array (map c/complex (range 64))))

(defn diff [arr normal fast]
  (let [a (normal arr)
        b (fast arr)]
    (->> (map #(.abs (c/- %1 %2)) a b)
         (reduce +))))

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

(t/deftest fft
  (t/is (> 1e-5 (diff ex-arr16 (sut/fft-slow -) (sut/fft-w-miejscu-czas -))))
  (t/is (> 1e-5 (diff ex-arr64 (sut/fft-slow -) (sut/fft-w-miejscu-czas -))))
  (t/is (> 1e-5 (diff ex-arr16 (sut/fft-slow +) (sut/fft-w-miejscu-czas +))))
  (t/is (> 1e-5 (diff ex-arr64 (sut/fft-slow +) (sut/fft-w-miejscu-czas +)))))

(def test-sig (s/fop +
                     {:fun :sin :period 0.128 :end 0.128}
                     {:fun :sin :period 0.064 :phase 0.5 :end 0.128}
                     {:fun :sin :period 0.032 :phase 0.25 :end 0.128}))

(comment
  (binding [g/*together* false g/*magnitude* false] (g/show (sut/przebrandzluj (sut/fft-w-miejscu-częstotliwość -) test-sig))))

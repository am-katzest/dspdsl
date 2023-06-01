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
(t/deftest wh
  (t/is (= '(4.0 2.0 0.0 -2.0 0.0 2.0 0.0 2.0) (map #(* (Math/sqrt 2) 2 %) (map c/real-part (into [] (sut/walsh-slow (into-array (map c/complex '(1, 0, 1, 0, 0, 1, 1, 0))))))))))
(t/deftest fast-same-as-slow
  (t/is (> 1e-5 (diff ex-arr16 (sut/ft-slow -) (sut/fft-w-miejscu-czas -))))
  (t/is (> 1e-5 (diff ex-arr64 (sut/ft-slow -) (sut/fft-w-miejscu-czas -))))
  (t/is (> 1e-5 (diff ex-arr16 (sut/ft-slow +) (sut/fft-w-miejscu-czas +))))
  (t/is (> 1e-5 (diff ex-arr64 (sut/ft-slow +) (sut/fft-w-miejscu-czas +))))
  (t/is (> 1e-5 (diff ex-arr64 sut/kos-slow sut/kos-fast)))
  (t/is (> 1e-5 (diff ex-arr64 sut/kos-slow-rev sut/kos-fast-rev))))

(t/deftest reversing-each-other
  (t/is (> 1e-5 (diff ex-arr64 identity (comp (sut/fft-w-miejscu-czas +) (sut/fft-w-miejscu-czas -)))))
  (t/is (> 1e-5 (diff ex-arr64 identity (comp (sut/fft-w-miejscu-częstotliwość +) (sut/fft-w-miejscu-częstotliwość -)))))
  (t/is (> 1e-5 (diff ex-arr64 identity (comp  sut/kos-slow-rev sut/kos-slow))))
  (t/is (> 1e-5 (diff ex-arr64 identity (comp  sut/kos-fast-rev sut/kos-fast)))))
(def test-sig (s/fop +
                     {:fun :sin :period 0.128 :end 0.128}
                     {:fun :sin :period 0.064 :phase 0.5 :end 0.128}
                     {:fun :sin :period 0.032 :phase 0.25 :end 0.128}))

(t/deftest xyxy
  (t/is (= [0 2 4 6 7 5 3 1] (->> 8 range object-array sut/x->y (into []))))
  (t/is (= [0 7 1 6 2 5 3 4] (->> 8 range object-array sut/y->x (into []))))
  (t/is (= (range 8) (->> 8 range object-array sut/x->y sut/y->x (into []))))
  (t/is (= (range 32) (->> 32 range object-array sut/x->y sut/y->x (into [])))))

(comment
  (binding [g/*together* false g/*magnitude* false] (g/show (sut/przebrandzluj (sut/fft-w-miejscu-częstotliwość -) test-sig)))
  (g/show (sut/przebrandzluj sut/kos-fast-rev (sut/przebrandzluj sut/kos-fast test-sig)))
  (g/show (s/discrete test-sig)))

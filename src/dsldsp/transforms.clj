(ns dsldsp.transforms
  (:require [complex.core :as c]
            [dsldsp.signal :as s]
            [dsldsp.graph :as g]))

(def e (c/complex Math/E))
(def j (c/complex 0 1))
(def -j (c/complex 0 -1))

(defn F-1 [x]
  (let [N (count x)]
    (amap x m _
          (areduce x n ret 0.0
                   (c/+ ret (c/*  (aget x n) (c/pow e (c// (c/* -j 2.0 Math/PI m n) N))))))))
(defn F-2 [x]
  (let [N (count x)]
    (amap x m _
          (areduce x n ret 0.0
                   (c/+ ret (c/*  (aget x n) (c/pow e (c// (c/*  j 2.0 Math/PI m n) N))))))))

;; stuff in this namespace takes 2^n inputs, as arrays of  complex numbers
(defn przebrandzluj [f x]
  (let [{:keys [values imaginary] :or {imaginary (repeat 0.)} :as dis} (s/discrete x)
        formatted (into-array (map c/complex values imaginary))
        result (into [] (f formatted))]
    (println (count values))
    (assoc dis
           :values (mapv c/real-part result)
           :imaginary (mapv c/imaginary-part result))))
(def arr (object-array (map c/+ (range 16))))
(map (juxt c/real-part c/imaginary-part) (F-1 arr))
(g/show (przebrandzluj F-1 (s/discrete {:fun :noise :period 1/300 :end 0.128})))
(g/show (przebrandzluj F-2 (przebrandzluj F-1 (s/discrete {:fun :sin :period 1/30 :end 0.128}))))

(def test-sig (s/fop +
                     {:fun :sin :period 0.128 :end 0.128}
                     {:fun :sin :period 0.064 :end 0.128}
                     {:fun :sin :period 0.032 :end 0.128}))
(g/show (przebrandzluj F-2 (przebrandzluj F-1 test-sig)))
(g/show (przebrandzluj F-1 test-sig))

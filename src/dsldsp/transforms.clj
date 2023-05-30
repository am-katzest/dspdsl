(ns dsldsp.transforms
  (:require [complex.core :as c]
            [dsldsp.signal :as s]
            [dsldsp.graph :as g]
            [better-cond.core :as b]))

(def e (c/complex Math/E))
(def j (c/complex 0 1))
(def -j (c/complex 0 -1))
(definline W [N x]
  `(c/pow e (c// (c/* j 2.0 Math/PI ~x) ~N)))
(defn F-1 [x]
  (let [N (count x)]
    (amap x m _
          (areduce x n ret 0.0
                   (c/+ ret (c/*  (aget x n) (W N (c/* (c/- m) n))))))))

(defn F-2 [x]
  (let [N (count x)]
    (amap x m _
          (areduce x n ret 0.0
                   (c/+ ret (c/*  (aget x n) (W N (c/* m n))))))))

;; stuff in this namespace takes 2^n inputs, as arrays of  complex numbers
(defn przebrandzluj [f x]
  (let [{:keys [values imaginary] :or {imaginary (repeat 0.)} :as dis} (s/discrete x)
        formatted (into-array (map c/complex values imaginary))
        result (into [] (f formatted))]
    (println (count values))
    (assoc dis
           :values (mapv c/real-part result)
           :imaginary (mapv c/imaginary-part result))))
;; śmieci
(def arr (object-array (map c/+ (range 16))))

(def test-sig (s/fop +
                     {:fun :sin :period 0.128 :end 0.128}
                     {:fun :sin :period 0.064 :end 0.128}
                     {:fun :sin :period 0.032 :end 0.128}))
;; (g/show (przebrandzluj F-2 (przebrandzluj F-1 test-sig)))
;; (g/show (przebrandzluj F-1 test-sig))
;; (g/show (przebrandzluj F-1 {:fun :triangle :end 0.128 :period 0.016}))

(defn rev-int [^long n ^long x]
  (unsigned-bit-shift-right (Long/reverse x) (- 64 n)))

(definline aswap [a i j]
  `(let [t# (aget ~a ~i)]
     (aset ~a ~i (aget ~a ~j))
     (aset ~a ~j t#)))

(defn fft-w-miejscu [old]
  (let [x (object-array old)
        N (count x)
        bity (long (/ (Math/log N) (Math/log 2)))]
    (dotimes [i (/ N 2)]
      (let [j (rev-int bity i)]
        (when (not= i j) (aswap x i j))))
    x))
(time (let [x (long-array (range 1024))]
        (dotimes [_ 555]
          (fft-w-miejscu x))))

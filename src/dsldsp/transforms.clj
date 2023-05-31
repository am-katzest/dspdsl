(ns dsldsp.transforms
  (:import [org.apache.commons.math3.complex Complex])
  (:require [complex.core :as c]
            [dsldsp.signal :as s]
            [dsldsp.graph :as g]
            [better-cond.core :as b]))

(def e (c/complex Math/E))
(def j (c/complex 0 1))
(def -j (c/complex 0 -1))
(definline W [N x]
  `(c/pow e (c// (c/* -j 2.0 Math/PI ~x) ~N)))
(defn F-1 [x]
  (let [N (count x)]
    (amap ^long x m _
          (areduce ^long x n ret 0.0
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

(defn rev-int [^long n ^long x]
  (unsigned-bit-shift-right (Long/reverse x) (- 64 n)))

(definline aswap [a i j]
  `(let [t# (aget ~a ~i)]
     (aset ~a ~i (aget ~a ~j))
     (aset ~a ~j t#)))

(defn fuck [^long off ^long N thing]
  (when (> N 2)
    (let [hn (/ N 2)]
      (fuck off hn thing)
      (fuck (+ off hn) hn thing)))
  (thing off N))

(defn fft-w-miejscu [old]
  (let [x (object-array old)
        N (count x)
        bity (long (/ (Math/log N) (Math/log 2)))]

    (dotimes [i N]
      (let [j (rev-int bity i)]
        (when (>= i j) (aswap x i j))))

    (fuck 0 N (fn [^long off ^long N]
                (dotimes [m (/ N 2)]
                  (let [n (+ m (/ N 2))
                        idx (fn [^long x] (+ off x))
                        nvw (c/* (aget x (idx n)) (W N (- m)))
                        mv (aget x (idx m))]
                    ;; (println "lokacje:" [(+ off m) (+ off n)] [(idx m) (idx n)] "W(" N "," (- m) ")")
                    (aset x (idx m) (c/+ mv  nvw))
                    (aset x (idx n) (c/- mv  nvw))))))
    x))

(comment
  (time (let [x (long-array (range 1024))]
          (dotimes [_ 55]
            (fft-w-miejscu x)))))
;; śmieci
(def arr (object-array (map c/+ (range 8))))
(into [] (fft-w-miejscu arr))
(def test-sig (s/fop +
                     {:fun :sin :period 0.128 :end 0.128}
                     {:fun :sin :period 0.064 :phase 0.5 :end 0.128}
                     {:fun :sin :period 0.032 :phase 0.25 :end 0.128}))
;; (g/show (przebrandzluj F-2 (przebrandzluj F-1 test-sig)))
;; (g/show (przebrandzluj F-1 test-sig))
;; (g/show (przebrandzluj fft-w-miejscu (przebrandzluj fft-w-miejscu test-sig)))
(g/show (przebrandzluj fft-w-miejscu test-sig))
;; (g/show (przebrandzluj F-1 {:fun :triangle :end 0.128 :period 0.016}))
(into [] (fft-w-miejscu (into-array (range 16))))

(defmacro runtime
  [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn stat [f n]
  (for [x (take n (iterate #(* 2 %) 2))
        :let [tst (->> x range (map c/+) object-array)
              time (runtime (f tst))]]
    time))
(defn stas [xs]
  (map (fn [[x y]] [y (float (/ y x))]) (cons [(first xs) (first xs)]
                                              (partition 2 1 xs))))

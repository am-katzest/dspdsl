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

(defn rev-int [^long n ^long x]
  (unsigned-bit-shift-right (Long/reverse x) (- 64 n)))

(definline aswap [a i j]
  `(let [t# (aget ~a ~i)]
     (aset ~a ~i (aget ~a ~j))
     (aset ~a ~j t#)))

(definline motylek [off a m N]
  `(let [m# ~m
         n# (+ m# (/ ~N 2))
         nwv# (c/* (aget ~a (+ ~off n#)) (W ~N (- m#)))
         mv# (aget ~a (+ ~off m#))]
     (println (+ ~off m#) (+ ~off n#) ~N "--" (- m#))
     (aset ~a (+ ~off m#) (c/+ mv#  nwv#))
     (aset ~a (+ ~off n#) (c/- mv#  nwv#))))
(definline motylek-rev [off a m N]
  `(let [m# ~m
         n# (+ m# (/ ~N 2))
         nwv# (c/* (aget ~a (+ ~off n#)) (W ~N m#))
         mv# (aget ~a (+ ~off m#))]
     (aset ~a (+ ~off m#) (c/+ mv#  nwv#))
     (aset ~a (+ ~off n#) (c/- mv#  nwv#))))

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
    (dotimes [i (/ N 2)]
      (let [j (rev-int bity i)]
        (when (not= i j) (aswap x i j))))
    (fuck 0 N (fn [^long off ^long N]
                (dotimes [m (/ N 2)]
                  (let [n (+ m (/ N 2))]
                    (println "--" off m n))
                  (motylek off x m N))))
    x))
(defn fft-w-miejscu-rev [old]
  (let [x (object-array old)
        N (count x)
        bity (long (/ (Math/log N) (Math/log 2)))]
    (dotimes [i (/ N 2)]
      (let [j (rev-int bity i)]
        (when (not= i j) (aswap x i j))))
    (fuck 0 N (fn [^long off ^long N]
                (dotimes [m (/ N 2)]
                  ;; (let [n (+ m (/ N 2))]
                  ;;   (println "--" off m n))
                  (motylek-rev off x m N))))
    x))
(comment
  (time (let [x (long-array (range 1024))]
          (dotimes [_ 55]
            (fft-w-miejscu x)))))

;; śmieci
(def arr (object-array (map c/+ (range 8))))

(def test-sig (s/fop +
                     {:fun :sin :period 0.128 :end 0.128}
                     {:fun :sin :period 0.064 :end 0.128}
                     {:fun :sin :period 0.032 :end 0.128}))
;; (g/show (przebrandzluj F-2 (przebrandzluj F-1 test-sig)))
;; (g/show (przebrandzluj F-1 test-sig))
;; (g/show (przebrandzluj fft-w-miejscu-rev (przebrandzluj fft-w-miejscu test-sig)))
;; (g/show (przebrandzluj fft-w-miejscu test-sig))
;; (g/show (przebrandzluj F-1 {:fun :triangle :end 0.128 :period 0.016}))
(fft-w-miejscu arr)
(W 8 6)

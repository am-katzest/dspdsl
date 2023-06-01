(ns dsldsp.transforms
  (:import [org.apache.commons.math3.complex Complex])
  (:require [complex.core :as c]
            [dsldsp.signal :as s]
            [dsldsp.graph :as g]
            [better-cond.core :as b]))

(defn przebrandzluj [f x]
  (let [{:keys [values imaginary] :or {imaginary (repeat 0.)} :as dis} (s/discrete x)
        formatted (into-array (map c/complex values imaginary))
        result (into [] (f formatted))]
    (println (count values))
    (assoc dis
           :values (mapv c/real-part result)
           :imaginary (mapv c/imaginary-part result))))

(def e (c/complex Math/E))
(def j (c/complex 0 1))
(def -j (c/complex 0 -1))

(definline W [N x]
  `(c/pow e (c// (c/* -j 2.0 Math/PI ~x) ~N)))

;; stuff in this namespace takes 2^n inputs, as arrays of  complex numbers

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

(defn create-lookup [op N]
  (->> (/ N 2) range (map #(W N (op %)))))

(defn- do-the-fft-shuffle-thing [^long N ^long bity ^"objects" x]
  (dotimes [i N]
    (let [j (rev-int bity i)]
      (when (>= i j) (aswap x i j))))
  x)

(defn- scale [^long N  ^"objects" x]
  (dotimes [i N]
    (aset x i (c// (aget x i) N)))
  x)

;; fourier

(defn ft-slow [op]
  (fn [x] (let [x (object-array x)
                N (count x)
                ans  (amap ^long x m _
                           (areduce ^long x n ret 0.0
                                    (c/+ ret (c/*  (aget x n) (W N (* (op m) n))))))]
            (when (= op -) (scale N ans))
            ans)))
(defn fft-w-miejscu-czas [op]
  (fn [old] (let [x (object-array old)
                  GN (count x)
                  bity (long (/ (Math/log GN) (Math/log 2)))]
              (do-the-fft-shuffle-thing GN bity x)
              (doseq [^long N (take bity (iterate #(* 2 %) 2))
                      :let [lookup (object-array (create-lookup op N))]
                      ^long off (range 0 GN N)]
                (dotimes [m (/ N 2)]
                  (let [^long n (+ m (/ N 2))
                        ^Complex nvw (c/* (aget x (+ off n)) (aget lookup m))
                        ^Complex mv (aget x (+ off m))]
                    (aset x (+ off m) (c/+ mv  nvw))
                    (aset x (+ off n) (c/- mv  nvw)))))
              (when (= op -) (scale GN x))
              x)))
(comment
  (time (let [x (object-array (map c/complex (range 1024)))]
          (dotimes [_ 55]
            ((fft-w-miejscu-czas -) x)))))

(defn fft-w-miejscu-częstotliwość [op]
  (fn [old] (let [x (object-array old)
                  GN (count x)
                  bity (long (/ (Math/log GN) (Math/log 2)))]
              (doseq [^long N (reverse (take bity (iterate #(* 2 %) 2)))
                      :let [lookup (object-array (create-lookup op N))]
                      ^long off (range 0 GN N)]
                (dotimes [m (/ N 2)]
                  (let [^long n (+ m (/ N 2))
                        ^Complex nv (aget x (+ off n))
                        ^Complex mv (aget x (+ off m))]
                    (aset x (+ off m) (c/+ mv  nv))
                    (aset x (+ off n) (c/* (c/- mv  nv) (aget lookup m))))))
              (when (= op -) (scale GN x))
              (do-the-fft-shuffle-thing GN bity x)
              x)))

;; śmieci

(defmacro runtime
  [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn stat [f r n]
  (for [x (take n (iterate #(* 2 %) 2))
        :let [tst (->> x range (map c/+) object-array)
              time (runtime (dotimes [_ r] (f tst)))]]
    time))

(defn stas [xs]
  (map (fn [[x y]] [y (float (/ y x))]) (cons [(first xs) (first xs)]
                                              (partition 2 1 xs))))

(definline c [N m]
  `(Math/sqrt (/ (if (zero? ~m) 1 2) ~N)))

(defn kos-slow [x]
  (let [x (object-array x)
        N (count x)]
    (amap ^long x m _
          (areduce ^long x n ret 0.0
                   (c/+ ret (c/*
                             (c N m)    ;nie było we wzorze
                             (aget x n)
                             (Math/cos (/ (* Math/PI (+ (* 2 n) 1) m)
                                          (* 2 N)))))))))
(defn kos-slow-rev [x]
  (let [x (object-array x)
        N (count x)]
    (amap ^long x n _
          (areduce ^long x m ret 0.0
                   (c/+ ret (c/*
                             (c N m)
                             (aget x m)
                             (Math/cos (/ (* Math/PI (+ (* 2 n) 1) m)
                                          (* 2 N)))))))))

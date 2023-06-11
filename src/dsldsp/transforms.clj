(ns dsldsp.transforms
  (:import [org.apache.commons.math3.complex Complex])
  (:require [complex.core :as c]
            [dsldsp.signal :as s]
            [dsldsp.graph :as g]
            [dsldsp.convolution :as conv]
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
  `(c/exp (c// (c/* -j 2.0 Math/PI ~x) ~N)))

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

(defn- scale
  ([^long N  ^"objects" x]
   (scale N (/ N) x))
  ([^long N s  ^"objects" x]
   (dotimes [i N]
     (aset x i (c/* (aget x i) s)))
   x))

;; fourier

(defn ft-slow [op]
  (fn [x] (let [x (object-array x)
                N (count x)
                ans  (amap ^long x m _
                           (areduce ^long x n ret 0.0
                                    (c/+ ret (c/*  (aget x n) (W N (* (op m) n))))))]
            (when (= op -) (scale N ans))
            ans)))

(defn- count-bits [N]
  (long (/ (Math/log N) (Math/log 2))))

(defn- fft-w-miejscu-czas [op]
  (fn [old] (let [x (object-array old)
                  GN (count x)
                  bity (count-bits GN)]
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

(def fft-time-fast (fft-w-miejscu-czas -))
(def fft-time-fast-rev (fft-w-miejscu-czas +))

(defn- fft-w-miejscu-częstotliwość [op]
  (fn [old] (let [x (object-array old)
                  GN (count x)
                  bity (count-bits GN)]
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

(def fft-freq-fast (fft-w-miejscu-częstotliwość -))
(def fft-freq-fast-rev (fft-w-miejscu-częstotliwość +))



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
(defn x->y [^"objects" x]
  (let [N (count x)
        y (object-array N)]
    (dotimes [i (/ N 2)]
      (aset y i (aget x (* 2 i))))
    (dotimes [i (/ N 2)]
      (aset y (- N i 1) (aget x (inc (* 2 i)))))
    y))
(defn y->x [^"objects" y]
  (let [N (count y)
        x (object-array N)]
    (dotimes [i (/ N 2)]
      (aset x (* 2 i) (aget y i)))
    (dotimes [i (/ N 2)]
      (aset x  (inc (* 2 i)) (aget y (- N i 1))))
    x))

(definline Re [x]
  `(c/complex (c/real-part ~x)))

(defn kos-fast [^"objects" x]
  (let [N (count x)
        ^"objects" dft ((fft-w-miejscu-czas +) (x->y x))]
    (amap dft m ret (Re (c/* (c N m) (c/exp (c/* -j Math/PI m (/ 1 2 N))) (aget dft m))))))

(defn kos-fast-rev [^"objects" x]
  (let [N (count x)
        a (amap x m ret (c/* (c N m) (c/exp (c/* j Math/PI m (/ 1 2 N))) (aget x m)))
        b ((fft-w-miejscu-czas -) a)
        c (amap b i ret (Re (aget b i)))]
    (y->x (scale N N c))))

(defn  make-wh-array [m]
  (let [N (int (Math/pow 2 m))
        arr (make-array Double/TYPE N N)]
    (aset arr 0 0  (/ (Math/pow 2 (/ m 2))))
    (doseq [r (take m (iterate #(* % 2) 1))]
      (dotimes [i r]
        (dotimes [j r]
          (let [^double  x (aget arr i j)]
            (aset arr (+ r i) j x)
            (aset arr i (+ r j) x)
            (aset arr (+ r i) (+ r j) (- x))))))
    arr))

(defn multiply [^"objects" v ^"objects" m]
  (amap v j ret
        (let [^"doubles" mr (aget m j)]
          (areduce mr i acc 0.0 (c/+ acc (c/* (aget v i) (aget mr i)))))))

(defn wht-slow [^"objects" x]
  (let [N (count x)
        b (count-bits N)
        m (make-wh-array b)]
    (multiply x m)))

(defn wht-fast [x]
  (let [a (object-array x)
        N (count a)
        b (count-bits N)
        scale (/ (Math/pow 2 (/ b 2)))]
    (doseq [h (take b (iterate #(* % 2) 1))
            i (range 0 N (* h 2))
            j (range i (+ i h))
            :let [x (aget a j)
                  y (aget a (+ j h))]]
      (aset a j (c/+ x y))
      (aset a (+ j h) (c/- x y)))
    (amap a i ret (c/* scale (aget a i)))))
(def H [0.47046721, 1.14111692, 0.650365, -0.19093442, -0.12083221, 0.0498175])
(def HDB6-H (double-array H))
(def HDB6-G (double-array (map-indexed (fn [n i] (if (even? n) i (- i))) (reverse H))))
(defn falk [H G X cnt]
  (let [N (count X)
        xh (conv/convolute-cut H X)
        xg (conv/convolute-cut G X)
        x1 (double-array (/ N 2))
        x2 (double-array (/ N 2))]
    (dotimes [i (/ N 2)]
      (aset x1 i (aget xh (* i 2)))
      (aset x2 i (aget xg (inc (* i 2)))))
    (cnt N [x1 x2])))
;; (defn cont-rec [N [x1 x2]]
;;   (if  (> N 2)))

(defn unfalk [D R [x1 x2]]
  (let [x1 (if (vector? x1) (unfalk D R x1) x1)
        x2 (if (vector? x2) (unfalk D R x2) x2)
        N (* 2 (count x1))
        xd (double-array N 0.)
        xr (double-array N 0.)]
    (dotimes [i (/ N 2)]
      (aset xd (* i 2) (aget x1 i))
      (aset xr (inc (* i 2)) (aget x2 i)))
    (let [d (conv/convolute-cut D xd)
          r (conv/convolute-cut R xr)]
      (amap r i ret (+ (aget r i) (aget d i))))))
(def xxs (s/dop + {:fun :sin :period 0.05 :end 0.128} {:fun :noise-gauss :amplitude 0.1 :end 0.128}))

(g/show xxs)
(g/show (s/wrap-discrete (into [] (first (falk HDB6-H HDB6-G (double-array (:values xxs)) (fn [_ a] a))))))
(g/show (s/wrap-discrete (into [] (second (falk HDB6-H HDB6-G (double-array (:values xxs)) (fn [_ a] a))))))
;┌──────────────┐
;│ benchmarking │
;└──────────────┘
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

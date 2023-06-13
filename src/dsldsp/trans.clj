(ns dsldsp.trans
  (:require [dsldsp.transforms :as tran]
            [dsldsp.signal :as s]
            [complex.core :as c]))

(defmacro ^:private runtime
  [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (double (- (. System (nanoTime)) start#)) 1000000.0)))

(defn perf-stat [f r n]
  (for [x (take n (iterate #(* 2 %) 2))
        :let [tst (->> x range (into []) s/wrap-discrete)
              time (runtime (dotimes [_ r] (f tst)))]]
    time))

;; (defn stas [xs]
;;   (map (fn [[x y]] [y (float (/ y x))]) (cons [(first xs) (first xs)]
;;                                               (partition 2 1 xs))))

(defn- get-signal [{:keys [values imaginary duration] :or {imaginary (repeat 0.)}}]
  (let [pow2size (int (Math/pow 2 (tran/count-bits duration)))
        formatted (into-array (take pow2size (map c/complex values imaginary)))]
    formatted))

(defn- get-signal-real [{:keys [values duration]}]
  (let [pow2size (int (Math/pow 2 (tran/count-bits duration)))
        formatted (double-array (take pow2size values))]
    formatted))

(defn- przebrandzluj [f]
  (fn [x] (let [dis (s/discrete x)
                formatted (get-signal dis)
                result (into [] (f formatted))]
            (assoc dis
                   :values (mapv c/real-part result)
                   :imaginary (mapv c/imaginary-part result)))))
(defn wavelet [x]
  (let [dis (s/discrete x)
        formatted (get-signal-real dis)
        [_ [a b]] (tran/falk tran/HDB6-H tran/HDB6-G formatted vector)
        undo (fn [x] (assoc (dissoc dis :imaginary)
                            :duration (count x)
                            :values (vec x)))]
    [(undo a)
     (undo b)]))

(def fourier-slow (przebrandzluj (tran/ft-slow -)))
(def fourier-slow-rev (przebrandzluj (tran/ft-slow +)))

(def fourier-fast (przebrandzluj (tran/fft-w-miejscu-czas -)))
(def fourier-fast-rev (przebrandzluj (tran/fft-w-miejscu-czas +)))

(def fourier-frequency-fast (przebrandzluj (tran/fft-w-miejscu-częstotliwość -)))
(def fourier-frequency-fast-rev (przebrandzluj (tran/fft-w-miejscu-częstotliwość +)))

(def kosinus-slow (przebrandzluj tran/kos-slow))
(def kosinus-slow-rev (przebrandzluj tran/kos-slow-rev))

(def kosinus-fast (przebrandzluj tran/kos-fast))
(def kosinus-fast-rev (przebrandzluj tran/kos-fast-rev))

(def hadamard-slow (przebrandzluj tran/wht-slow))
(def hadamard-fast (przebrandzluj tran/wht-fast))

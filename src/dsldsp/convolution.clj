(ns dsldsp.convolution)

(defn convolute "h * x" [h x]
  (let [M (count h)
        N (count x)
        H (double-array h)
        X (double-array x)
       ;; going lower than usual to get efficient bytecode
        at-point (fn [^long n]
                   (let [end (min n (dec M))
                         start (max 0 (- n N -1))]
                     (loop [k start acc 0.0]
                       (if (> k end) acc
                           (recur (inc k)
                                  (+ acc (*
                                          (aget H k)
                                          (aget X (- n k)))))))))]
    (->> (range (+ M N -1))
         (mapv at-point))))
(defn convolute-cut "h * x" [^"doubles" H ^"doubles" X]
  (let [M (count H)
        N (count X)
        at-point (fn [^long n]
                   (let [end (min n (dec M))
                         start (max 0 (- n N -1))]
                     (loop [k start acc 0.0]
                       (if (> k end) acc
                           (recur (inc k)
                                  (+ acc (*
                                          (aget H k)
                                          (aget X (- n k)))))))))]
    (->> (range N)
         (mapv at-point)
         (double-array))))
(vec (convolute-cut (double-array [1 2]) (double-array [1 2 3 4])))
(time
 (let [x (repeat 1511 3.0)]
   (dotimes [_ 5]
     (convolute x x))))

;; (defn correlate [h x]
;;   (convolute h (vec (reverse x))))

(defn correlate "h * x" [h x]
  (let [M (count h)
        N (count x)

        H (double-array h)
        X (double-array x)
        ;; going lower than usual to get efficient bytecode
        at-point (fn [^long n]
                   (let [end (min n (dec M))
                         start (max 0 (- n N -1))]
                     (loop [k start acc 0.0]
                       (if (> k end) acc
                           (recur (inc k)
                                  (+ acc (*
                                          (aget H k)
                                          (aget X (- N 1 (- n k))))))))))]
    (->> (range (+ M N -1))
         (mapv at-point))))

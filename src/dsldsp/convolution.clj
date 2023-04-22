(ns dsldsp.convolution)

(defn convolute "h * x" [h x]
  (let [M (count h)
        N (count x)
        H (double-array h)
        X (double-array x)
       ;; going lower than usual to get efficient bytecode
        convolution-at-point (fn [n]
                               (let [end (min n (dec M))
                                     start (max 0 (- n N -1))]
                                 (loop [k start acc 0.0]
                                   (if (> k end) acc
                                       (recur (inc k)
                                              (+ acc (*
                                                      (aget H k)
                                                      (aget X (- n k)))))))))]
    (binding [*unchecked-math* false]
      (->> (range (+ M N -1))
           (mapv convolution-at-point)))))

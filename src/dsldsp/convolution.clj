(ns dsldsp.convolution)

(defn convolute "a * b, assumes equal sampling frequency" [a b]
  (let [al (count a)
        bl (count b)
        a- (double-array a)
        b- (double-array b)]
       ;; going lower than usual to get efficient bytecode
    (binding [*unchecked-math* false]
      (->> (range (- 1 bl) al)
           (mapv
            (fn [off]
              (let [end (min bl (- al off))
                    start (max (- off) 0)]
                (loop [j start acc 0.0]
                  (if (>= j end) acc
                      (recur (inc j)
                             (+ acc (* ;; (get a (+ off j) 0)
                                     (aget a- (+ off j))
                                     (aget b- j)))))))))))))

(ns dsldsp.core
  (:require [dsldsp.signal :as s]
            [dsldsp.graph :as g]))
;; fancy are a second-class citizens, everything complex is done via discrete
(comment
  ;; fourier thingy demo
  (g/show
   (apply s/fop +
          (for [x (range 2 555 2)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)})))
  (g/show
   (s/dop max
          {:function :sin :phase 0.50}
          {:function :sin})))

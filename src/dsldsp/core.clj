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
          {:function :sin}))
  (g/show
   (s/fop +
          {:function :triangle :fill 0.0 :start 0 :duration 2}
          {:function :triangle :fill 0.5 :start 4 :duration 2}
          {:function :triangle :fill 1.0 :start 8 :duration 2})))

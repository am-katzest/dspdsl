(ns dsldsp.core
  (:require [dsldsp.signal :as s]
            [dsldsp.graph :as g]))

(comment
  ;; fourier thingy demo
  (g/show
   (apply s/fop +
          (for [x (range 1 555 2)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)}))))

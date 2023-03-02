(ns dsldsp.core
  (:require [dsldsp.signal :as s]
            [dsldsp.graph :as g]
            [dsldsp.io :as i]))
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
   (s/dop +
          {:function :triangle :fill 0.0 :start 0 :duration 2}
          {:function :triangle :fill 0.5 :start 4 :duration 2}
          {:function :triangle :fill 1.0 :start 8 :duration 2}))

  (g/showboth (apply s/dop + (for [x (range 50)] (s/impulse-noise :p 0.5))))

  (let [uwu {:function :jump
             :start 10
             :fill 12
             :duration 10}]
    (i/write "uwu" uwu)
    (g/show uwu)
    (g/show (i/read "uwu")))

  (g/histogram {:function :square :duration 1.5})
  (g/stat {:function :triangle :duration 22.1}))

(keys s/functions-cont)

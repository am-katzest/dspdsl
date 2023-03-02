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
   (s/make-complex (apply s/dop max
                          (for [x (range 0 1 (/ 5))]
                            {:function :sin :phase x}))
                   (s/fop max
                          {:function :sin :phase 1/2}
                          {:function :sin})))

  (g/show
   (s/dop +
          {:function :triangle :fill 0.0 :start 0 :duration 2}
          {:function :triangle :fill 0.5 :start 4 :duration 2}
          {:function :triangle :fill 1.0 :start 8 :duration 2}))

  (g/show
   ;; UwU
   (s/discrete
    (let [v (s/fop -
                   {:function :const :amplitude 1 :start 0 :duration 1}
                   {:function :sin-half :start 0 :duration 1 :period 2})
          U (s/fop * v v v)
          w {:function :triangle :fill 0.5 :start 2.5 :duration 2 :amplitude 0.4}]
      (s/fop +
             (s/tshift U 1.5)
             (s/tshift U 4.5)
             (s/fop #(* % 1.6 (Math/sqrt %)) w)))))
  (g/graph "UwU")

  (g/showboth (apply s/dop + (for [x (range 50)] (s/impulse-noise :p 0.5))))

  (g/histogram {:function :square :duration 1.5})
  (g/stat {:function :triangle :duration 22.1}))

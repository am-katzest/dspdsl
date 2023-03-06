(ns dsldsp.core
  (:require [dsldsp.signal :as s :refer :all]
            [dsldsp.graph :as g :refer :all]
            [dsldsp.io :as i :refer :all]
            [complex.core :as c]))
;; fancy are a second-class citizens, everything complex is done via discrete
(comment

  ;; fourier thingy demo
  (show
   (apply fop +
          (for [x (range 2 555 2)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)})))

  (binding [hist-bins 20
            sampling-frequency 1/73]
    (show
     (make-complex (apply dop max
                          (for [x (range 0 1 (/ 20))]
                            {:function :sin :phase x}))
                   (fop max
                        {:function :sin :phase 1/2}
                        {:function :sin}))))

  (show
   (dop +
        {:function :triangle :fill 0.0 :start 0 :duration 2}
        {:fun :triangle :fill 0.5 :s 4 :d 2}
        {:fun :triangle :fill 1.0 :s 8 :e 10}))

  (show
   ;; UwU
   (let [v (fop -
                {:function :const :amplitude 1 :start 0 :duration 1}
                {:function :sin-half :start 0 :duration 1 :period 2})
         U (fop * v v v)
         w {:function :triangle :fill 0.5 :start 2.5 :duration 2 :amplitude 0.4}]
     (dop max
          (tshift U 1.5)
          (tshift U 4.5)
          (fop #(* % 1.6 (Math/sqrt %)) w))))
  (show "UwU")

  (histogram (apply dop + (for [x (range 50)] {:function :noise-impulse :fill 0.05})))

  (binding [sampling-frequency 1/500]
    (show (dop + {:function :sin :duration 0.1}
               (impulse :ns 2)
               (impulse :ns 4))))

  (show {:function :square :duration 1.5})

  (stat {:function :triangle :duration 22.1})

  (let [sig (make-complex {:f :sin}
                          {:f :sin :ph 1/2})]
    (show (dop c/* sig sig)))
  ;;
  )

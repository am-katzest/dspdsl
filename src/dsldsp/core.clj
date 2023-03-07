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
          (for [x (range 2 600 2)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)})))

  (binding [hist-bins 20
            sampling-period 1/73]
    (show
     (make-complex (apply dop max
                          (for [x (range 0 1 (/ 20))]
                            {:function :sin :phase x}))
                   (dop max
                        {:function :sin :phase 1/2}
                        {:function :sin}))))

  (show
   (dop +
        {:function :triangle :fill 0.0 :start 0 :duration 2}
        {:fun :triangle :fill 0.5 :s 4 :d 2}
        {:fun :triangle :fill 1.0 :s 8 :e 10}))

  {:basic {:type :discrete, :period 1.0, :sampling 0.05, :duration 101, :start 0}, :stats {:średnia-bezwzględna 0.6313751514675069, :średnia 3.808064974464287E-16, :moc-średnia 0.5000000000000009, :wariancja 0.6313751514675071, :wartość-skuteczna 0.7071067811865481}} (binding [sampling-period 0.05] (stat {:fun :sin}))

  (show
   ;; UwU
   (let [v (fop -
                {:fun :const :e 1}
                {:fun :sin-half :period 2 :e 1})
         U (fop * v v v)
         w {:fun :triangle :start 2.5 :len 2}]
     (dop max
          (tshift U 1.5)
          (tshift U 4.5)
          (fop #(* % 1/3 (Math/sqrt %)) w))))
  (show "UwU")

  (histogram
   (apply dop +
          (for [_ (range 500)] {:fun :noise-impulse :fill 0.5})))

  (binding [sampling-period 1/500]
    (show (dop + {:function :sin :duration 0.1}
               (impulse :ns 2)
               (impulse :ns 4))))

  (show {:function :square :duration 1.5})

  (stat {:function :triangle :duration 22.1})

  (let [sig (make-complex {:f :sin}
                          {:f :sin :ph 1/2})]
    (show (dop c/* sig sig)))
  ;;
  ;; kwantyzacja
  (let [x (fop + {:f :sin} {:f :sin :A 1/2 :p 1/2})]
    (show (make-complex (fop (kwant 1/5) x)
                        x)))

  ;; porównanie metod
  (binding [sampling-period 1/10]
    (let [x (fop + {:f :sin} {:f :sin :A 1/2 :p 1/2} {:f :sin :p 3})]
      (show (make-complex x
                          (rzędu-pierwszego (discrete x))))
      (show (make-complex x
                          (rzędu-zerowego (discrete x))))))

  (let [x {:f :sin :s -5 :e 5}
        n 2.6]
    (show (make-complex
           (sinc-0
            (binding [sampling-period (/ n)]
              (discrete x)) (int (* n 3)))
           x)))

  ;;
  )

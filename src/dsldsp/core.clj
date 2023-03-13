(ns dsldsp.core
  (:require [dsldsp.signal :as s :refer :all]
            [dsldsp.graph :as g :refer :all]
            [dsldsp.io :as i :refer :all]
            [complex.core :as c]))
;; fancy are a second-class citizens, everything complex is done via discrete

(comment

  (show
   (apply fop +
          (for [x (range 2 3000 4)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)})))
  (show
   (apply fop +
          (for [x (range 2 300 4)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)})))
  ;; fourier thingy demo

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

  (binding [graph-samples 10000] (show
    ;; UwU
                                  (let [v (fop -
                                               {:fun :const :e 1}
                                               {:fun :sin-half :period 2 :e 1})
                                        U (fop * v v v)
                                        w (tshift {:fun :triangle :phase 0.5 :period 1/2 :len 1} 2.5)]
                                    (dop max
                                         (tshift U 1.5)
                                         (tshift U 3.5)
                                         (fop #(* % 1/3 (Math/sqrt %)) w)))))
  (show "UwU")

  (histogram
   (apply dop +
          (for [_ (range 500)] {:fun :noise-impulse :fill 0.5})))

  (binding [sampling-period 1/500]
    (show (dop + {:function :sin :duration 0.1  :amplitude 3}
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
  (binding [sampling-period 1/200]
    (let [x {:f :sin :s -5 :e 5}
          sampled (binding [sampling-period 1/6] (discrete x))
          x (discrete x)]
      {:0 (other-stat x (rzędu-zerowego sampled))
       :1 (other-stat x (rzędu-pierwszego sampled))
       :sinc (other-stat x (sinc-0 sampled 35))}))

  (->> (for [[i f] (map-indexed vector [{:f :sin}
                                        {:f :sin-half}
                                        {:f :sin-double}
                                        {:f :square :fill 0.1}
                                        {:f :square}
                                        {:f :square :fill 0.9}
                                        {:f :triangle :fill 0.1}
                                        {:f :triangle}
                                        {:f :triangle :fill 0.9}
                                        {:f :square-sym}
                                        {:f :noise}
                                        {:f :noise-gauss}
                                        {:f :noise-impulse}])]
         (assoc f :start (* i 3) :duration 2))
       (apply fop + {:f :jump :fill 23 :s 0 :e 39 :a 4})
       show)
  ;;
  )
(comment "sprawko 2"
         "konwersja A/C"

         (def a (fop + {:f :sin}
                     {:f :sin :a 0.5 :period 0.5}))

         (binding [sampling-period 1/100]
           (show (discrete a)))

         (binding [sampling-period 1/10]
           (show (discrete a)))

         (show (make-complex
                a
                (fop (kwant 0.4) a)))
         (show (make-complex
                a
                (fop (kwant 0.1) a)))
         (binding [sampling-period 1/10]
           (show (make-complex
                  (discrete a)
                  (fop (kwant 0.4) a))))
         (def both (binding [sampling-period 1/10]
                     (discrete (fop (kwant 0.4) a))))

         (def only (binding [sampling-period 1/10]
                     (discrete a)))

         (show (make-complex a (rzędu-zerowego only)))

         (show (make-complex a (sinc-0 only 20)))

         (show (make-complex a (sinc-0 only 80)))

         (show (make-complex a (sinc-0 both 80))))

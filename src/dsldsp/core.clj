(ns dsldsp.core
  (:require [dsldsp.signal :as ss :refer :all]
            [dsldsp.graph :as g :refer :all]
            [dsldsp.io :as i :refer :all]
            [complex.core :as c]))
;; fancy are a second-class citizens, everything complex is done via discrete
(defn showf [a b] (graph (make-complex (fancy a) (fancy b))))
(defn showw [a b] (graph (make-complex a b)))
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

  (binding [sampling-period 0.05] (stat {:fun :sin}))

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

  (write "sig" {:f :sin :period 1/100 :duration 1/10})
  (show "sig")
  (binding [sampling-period 1/10000]
    (write "sig2" {:period 1/20 :f :sin :duration 1/10 :A 5}))
  (show (dop + "sig" "sig2"))
  (show (discrete {:fun :noise-gauss}))
  (show {:fun :noise :A 3.5})
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

  (binding [sampling-period 1/100]
    (let [z (fop +  {:f :sin :spread true}
                 {:f :sin :spread true :A 0.5 :period 1/5}
                 {:f :noise :A 0.1 :spread true})
          x z
          n 30
          i1 (impulse :A (/ n) :len n :ns (- (/ n 2)))
          i2 (make-window n)]
      (showw z z)))

  (binding [sampling-period 1/100]
    (let [z (fop +
                 {:f :sin :period 1/55}
                 {:f :sin :period 1/20}
                 {:f :sin :period 1/5}
                 {:f :noise :A 0.1 :spread true})]
      (showw z (sinc-1 (convolute z (make-filter 50 20)) 20))
      (showw z (sinc-1 (convolute z (middle-pass (make-filter 50 10))) 20))
      (showw z (sinc-1 (convolute z (lower-pass (make-filter 50 10))) 20))))

;;
  )
(comment (binding [sampling-period 1/10
                   graph-samples 8000]
           (let [duration 20
                 cut 0.15
                 step 1/512
                 make (fn [f s] (tshift {:f :sin :spread true :A 1 :period (/ sampling-period f) :duration duration} s))
                 z (apply dop +
                          (for [[i x] (map-indexed vector (range step 1/2  step))]
                            (make x (* duration i))))
                                        ;to hide the weird things interlacing makes
                 mask  {:fun :square :start 0 :end 200 :period duration :fill (- 1 cut cut) :phase cut :spread true}
                 fix #(fop (fn [a b] (abs (* a b))) mask  (sinc-1 % 20))]
             (showw (fix z)
                    (fix (convolute z (middle-pass (make-filter 128 8))))))))
;; wyższe M po prostu poprawia jakość
;; K:
;; dolnoprzepustowy filtr przepuszcza częstotliwości poniżej 1/K dl FP=100Hz dla K=4 będzie przepuszczało sygnały poniżej 25HZ
;; pasmowy filtr przepuszcza częstotliwości
;; zawsze przepuszcza FP*4,
;; okno skaluje się odwrotnie proporcjonalnie z K
;; {}
(comment (map float (map / (range 1/32 1/2  1/32))))

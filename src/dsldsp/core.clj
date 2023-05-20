(ns dsldsp.core
  (:require [dsldsp.signal :as ss :refer :all]
            [dsldsp.graph :as g :refer :all]
            [dsldsp.io :as i :refer :all]
            [dsldsp.filter :as f :refer :all]
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
      (showw z (sinc-1 (convolute z (make-filter {:M 50 :K 20})) 20))
      (showw z (sinc-1 (convolute z (make-filter {:M 50 :K 10 :pass middle})) 20))
      (showw z (sinc-1 (convolute z (make-filter {:M 50 :K 10 :pass upper})) 20)))))

(comment
  ;; showcase filters
  (showf
   (filter-stat 1/128 1 (make-filter {:M 32 :K 8 :pass upper :window blackman}))
   (filter-stat 1/128 1 (make-filter {:M 64 :K 8 :pass lower})))
  (showf
   (filter-stat 1/128 1 (make-filter {:M 64 :K 8 :pass middle :window hanning}))
   (filter-stat 1/128 1 (make-filter {:M 64 :K 8 :pass middle}))))

;; wyższe M po prostu poprawia jakość
;; K:
;; dolnoprzepustowy filtr przepuszcza częstotliwości poniżej 1/K dl FP=100Hz dla K=4 będzie przepuszczało sygnały poniżej 25HZ
;; pasmowy filtr pdrzepuszcza częstotliwości
;; zawsze przepuszcza FP*4,
;; okno skaluje się odwrotnie proporcjonalnie z K
;; {}

(comment (defn- calc-delay [signal response]
           (float (- (max-time (correlate signal signal))
                     (max-time (correlate signal response)))))

         ;; detector thing
         (binding [sampling-period 1/100]
           (let [s (apply fop +
                          (for [x (range 0.5 10 1)]
                            {:fun :sin :spread true :A (/ x) :period (/ x) :start -5 :end 5}))
                 ;; s (discrete {:fun :noise :start -5 :end 5})
                 cutter #(cut % 0 1)
                 sig (cutter s)]
             (show (for [x (range 0 1 0.05)
                         :let [delayed (cutter (tshift s x))]]
                     (calc-delay sig delayed)))
             (showf sig (cutter (tshift s 0.9)))))
         (comment
           ;; convolution correlation showcase
           (let [f {:fun :const :end 1}
                 g {:fun :triangle :end 0.99999 :fill 0}]
             (show (correlate g g)))))

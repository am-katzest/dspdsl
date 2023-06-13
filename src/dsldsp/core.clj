(ns dsldsp.core
  (:require [dsldsp.signal :as ss :refer :all]
            [dsldsp.graph :as g :refer :all]
            [dsldsp.io :as i :refer :all]
            [dsldsp.filter :as f :refer :all]
            [dsldsp.trans :as t :refer :all]
            [complex.core :as c]))

(comment
  ;; pokaż funkcję
  (show {:fun :sin})
  ;; dodaj funkcje
  (show (fop + {:fun :sin} {:fun :sin :A 1/5 :period 1/5}))
  ;; współpraca z językiem
  (show
   (apply fop +
          (for [x (range 2 300 4)]
            {:function :sin
             :duration 1
             :period (/ x)
             :amplitude (/ x)})))

  ;; pokazywanie wielu funkcji jednocześnie, inne operatory, zmiana częstotliwości próbkowania
  (binding [*sampling-period* 1/730]
    (show-two
     (apply dop max
            (for [x (range 0 1 (/ 5))]
              {:function :sin :phase x}))
     (dop max
          {:function :sin :phase 1/2}
          {:function :sin})))

  ;; więcej opcji dot. sygnałów
  (show
   (dop -
        {:fun :triangle :fill 0.5 :s 4 :d 2}
        {:function :triangle :fill 0.0 :start 0 :duration 2}
        {:fun :triangle :fill 1.0 :s 8 :e 10}))

  ;; obliczanie statystyk
  (binding [*sampling-period* 0.05] (perf-stat {:fun :sin}))

;; zapisywanie do pliku
  (let [v (fop -
               {:fun :const :e 1}
               {:fun :sin-half :period 2 :e 1})
        U (fop * v v v)
        w (tshift {:fun :triangle :phase 0.5 :period 1/2 :len 1} 2.5)
        f (dop max
               (tshift U 1.5)
               (tshift U 3.5)
               (fop #(* % 1/3 (Math/sqrt %)) w))]
    (write "UwU" f))

  ;; odczytywanie z pliku
  (show "UwU")

  ;; histogram
  (histogram
   (apply dop +
          (for [_ (range 10)] {:fun :noise-impulse :fill 0.5})))

  ;; liczby złożone
  (histogram (make-complex {:fun :noise-gauss} {:fun :sin}))

  (write "complex" (make-complex {:fun :sin} {:fun :sin :phase 0.5}))
  (show (make-complex {:fun :sin} {:fun :sin :phase 0.5}))
  (show "complex")

  ;; impulsy
  (binding [*sampling-period* 1/100]
    (show (dop + {:function :sin :duration 0.1  :amplitude 3}
               (impulse :ns 2)
               (impulse :ns 4))))

  ;; kwantyzacja
  (let [x (fop + {:f :sin} {:f :sin :A 1/2 :p 1/2})]
    (show (make-complex (fop (kwant 1/5) x)
                        x)))

  ;; porównanie metod
  (binding [*sampling-period* 1/10]
    (let [x (fop + {:f :sin} {:f :sin :A 1/2 :p 1/2} {:f :sin :p 3})]
      (show-two x (rzędu-pierwszego (discrete x)))
      (show-two x (rzędu-zerowego (discrete x)))
      (show-two x (sinc-1 (discrete x) 5))))

  ;; numeryczne porównanie
  (binding [*sampling-period* 1/200]
    (let [x {:f :sin :s -5 :e 5}
          sampled (binding [*sampling-period* 1/6] (discrete x))
          x (discrete x)]
      {:0 (other-stat x (rzędu-zerowego sampled))
       :1 (other-stat x (rzędu-pierwszego sampled))
       :sinc35 (other-stat x (sinc-1 sampled 35))
       :sinc5 (other-stat x (sinc-1 sampled 5))}))

  ;; wszystkie funkcje
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
       show))

(comment
  ;; showcase filters
  (show-two
   (filter-stat 1/128 (make-filter {:M 32 :K 8 :pass upper :window blackman}))
   (filter-stat 1/128 (make-filter {:M 64 :K 8 :pass lower})))
  (show-two
   (filter-stat 1/256 (make-filter {:M 64 :K 8 :pass middle :window hanning}))
   (filter-stat 1/256 (make-filter {:M 64 :K 8 :pass middle})))
  (show-two
   (filter-stat 1/256 (make-filter {:M 20 :K 8 :pass middle :window hanning}))
   (filter-stat 1/256 (make-filter {:M 20 :K 8 :pass middle})))
  (show-two (filter-stat 1/128 (make-lower-pass-filter {:M 50} 200))
            (filter-stat 1/128 (make-upper-pass-filter {:M 50} 50)))
  (show-two
   (filter-stat 1/128 (make-pass-filter {:M 40 :window hanning} 200 400))
   (filter-stat 1/128 (make-pass-filter {:M 100 :window blackman} 100 300)))
  ;; filtry na praktycznej funkcji
  (binding [*sampling-period* 1/200]
    (let [z (dop +
                 {:f :sin :period 1/55}
                 {:f :sin :period 1/20}
                 {:f :sin :period 1/5})
          prepare #(cut (sinc-1 % 20) 2.5 2.7)
          compare #(show-two (prepare z) (prepare (convolute z %)))]
      (compare (make-lower-pass-filter {:M 80} 10))
      (compare (make-pass-filter {:M 80} 18 22))
      (compare (make-pass-filter {:M 80} 50 60)))))

(comment
  ;; konwolucja / korelacja
  (let [f {:fun :const :end 1}
        g {:fun :triangle :end 0.99999 :fill 0}]
    (show (correlate g f))))

;; korelacyjne wykrywanie odległości
(defn- calc-delay [signal response]
  (float (- (max-time (correlate signal signal))
            (max-time (correlate signal response)))))

(comment (binding [*sampling-period* 1/100]
           (let [;; s (apply fop +
                 ;;          (for [x (range 0.5 10 1)]
                 ;;            {:fun :sin :spread true :A (/ x) :period (/ x) :start -5 :end 5}))
                 s (discrete {:fun :noise-impulse :start 0 :end 0.1})
                 ;; s (discrete {:fun :sin :period 1/2 :start -5 :end 5})
                 ;;
                 cutter #(cut % 0 1)
                 sig (cutter s)]
             (show (wrap-discrete
                    (for [x (range 0 1 0.01)
                          :let [delayed (cutter (tshift (fop + {:fun :noise} s) x))]]
                      (calc-delay sig delayed))))
             (show-two sig (cutter (tshift s 0.9))))))

(comment
  ;; transformations
  (def sinusoids (binding [*sampling-period* 1/128]
                   (dop + {:fun :sin :e 1 :period 1/16}
                        {:fun :sin :e 1 :period 1/8 :phase 0.12}
                        {:fun :sin :e 1 :period 1/32})))
  (binding [*magnitude* false *together* false] (show (fourier-slow sinusoids))))

(comment
  (do (defn create-latex-table [caption data]
        (let [data (apply mapv vector data)
              num-cols (count (first data))]
          (println "\\begin{table}[H]")
          (println "\\centering")
          (println "\\caption{porównanie szybkości implementacji funkcji " caption " (czas w ms}")
          (println "\\begin{tabular}{|" (apply str (repeat num-cols "S|")) "} \\hline")
          (println (str (apply str (interpose " & " (map #(str "{" % "}") (first data)))) " \\\\ \\hline"))
          (doseq [row (rest data)]
            (println (str (apply str (interpose " & " row)) " \\\\ \\hline")))
          (println "\\end{tabular}")
          (println "\\end{table}")))

      (defn write-tbl [caption n r pairs]
        (->> pairs
             (map (fn [[title f]]
                    (cons title (perf-stat f r n))))
             (cons (cons "n" (map #(int (Math/pow 2 (inc %))) (range n))))
             (create-latex-table caption)
             with-out-str
             (spit (format "sprawko/%s.tex" caption)))))

  (with-out-str (write-tbl "fourier" 10 1 [["wolna" fourier-slow]
                                           ["szybka" fourier-fast]]))
  (with-out-str (write-tbl "kosinus" 10 1 [["wolna" kosinus-slow]
                                           ["szybka" kosinus-fast]]))
  (with-out-str (write-tbl "hadamard" 10 1 [["wolna" hadamard-slow]
                                            ["szybka" hadamard-fast]])))

(ns dsldsp.filter
  (:require [dsldsp.signal :refer :all]))

(defn- make-it-add-up-to-one [x]
  (let [sum (reduce + x)
        factor (/ sum)
        fixer (fn [x] (* factor x))]
    (mapv fixer x)))

(defn- make-sinc-filter [M K]
  (->> M
       range
       (mapv (fn [n]
               (let [thing (/ (- M 1) 2)
                     x (- n thing)]
                 (if (= thing n) (/ 2 K)
                     (/ (Math/sin (/ (* 2 Math/PI x)
                                     K))
                        Math/PI x)))))
       make-it-add-up-to-one))

(defn- idk-what-it-is [n j M] (Math/cos (* j Math/PI n (/ M))))
(defn- i-know-about-this-even-less [x1 & xs]
  (fn [M]
    (mapv (fn [n] (apply + x1 (for [[a j] xs]
                                (* a (idk-what-it-is n j M))))) (range M))))
(def hamming2
  (i-know-about-this-even-less 0.53836 [-0.46164 2]))

(defn hanning [M]
  (mapv (fn [n] (+ 1/2
                   (* -1/2 (Math/cos (* 2 Math/PI n (/ M)))))) (range M)))

(defn blackman [M]
  (mapv (fn [n] (+ 0.42
                   (* -0.5 (Math/cos (* 2 Math/PI n (/ M))))
                   (* 0.08 (Math/cos (* 4 Math/PI n (/ M)))))) (range M)))

(defn  square [M]
  (repeat M 1))

(defn add-window [window signal]
  (make-it-add-up-to-one (mapv * (window (count signal)) signal)))

(defn make-window [coll]
  {:type :discrete
   :sampling sampling-period
   :start (int (- (/ (count coll) 2)))
   :duration (count coll)
   :values coll})

(defn middle [window] (map * window (cycle [0 2 0 -2])))

(defn lower [window] (map * window (cycle [1 -1])))

(defn upper [window] window)

(defn make-filter [{:keys [M K pass window] :or {pass upper window square}}]
  (->>
   (make-sinc-filter M K)
   (add-window window)
   pass
   make-window))

(defn power [x]
  (let [{:keys [values]} (discrete x)]
    (->> values (map #(* % %)) (reduce +))))

(defn filter-stat [step duration filter]
  (binding [sampling-period 1/10]
    (let [make (fn [f] {:f :sin :spread true :A 1 :period (/ sampling-period f) :duration duration})
          fractions (range step 1/2  step) ;to hide the weird things interlacing makes
          pass #(convolute % filter)
          results (for [x fractions
                        :let [sig (make x)]]
                    (/ (power (pass sig)) (power sig)))]
      {:type :discrete :period 0 :sampling step :start 0 :values (vec results) :duration (count results)})))

(defn filter-stat- [step duration filter]
  (binding [sampling-period 1/10]
    (let [cut 0.1
          make (fn [f] {:f :sin :spread true :A 1 :period (/ sampling-period f) :duration duration})
          fractions (range step 1/2  step) ;to hide the weird things interlacing makes
          mask  {:fun :square :end  duration :period duration :fill (- 1 cut cut) :phase cut :spread true}
          pass #(convolute % filter)
          results (for [x fractions
                        :let [sig (make x)]]
                    (/ (power (dop * mask (pass sig))) (power (dop * mask sig))))]
      {:type :discrete :period 0 :sampling step :start 0 :values (vec results) :duration (count results)})))

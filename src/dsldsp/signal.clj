(ns dsldsp.signal
  (:import [cern.jet.random.tdouble Normal])
  (:require [clojure.math]))

(comment
  ;; signal examples, this is all docs you'll get ^w^
  '{:type :discrete :sampling (/ 1000) :start 0 :values [0 0.1 0.3 ...]}
  '{:type :fancy
    :start 0
    :stop 1
    :fun (fn [time] 0.0)}
  '{:type :spec
    :amplitude 1
    :period 1
    :start 0
    :duration 0.1
    :fill 0.1
    :function :square})

(defn max0 [x] (max x 0))
(defn min0 [x] (min x 0))
(defn div0 [& xs]
  (try (apply / xs)
       (catch java.lang.ArithmeticException _ 0)))

(def functions-cont
  ;; they are all normalized
  (let [sin (fn [{:keys [angle]}] (Math/sin (* angle 2.0 Math/PI)))]
    {:noise (fn [_] (- 1 (* 2 (rand))))
     :noise-gauss (fn [_] (Normal/staticNextDouble 1.0 1.0))
     :sin sin
     :const (fn [_] 1.)
     :triangle (fn [{:keys [angle fill]}] (if (> fill angle) angle 0))
     :square (fn [{:keys [angle fill]}] (if (> fill angle) 1 0))
     :square-sym (fn [{:keys [angle fill]}] (if (> fill angle) 1 -1))
     :sin-half (comp max0 sin)
     :sin-double (comp abs sin)
     :jump (fn [{:keys [time fill]}] (if (> time fill) 1 0))}))

(defn spec->fancy [{:keys [amplitude period start duration fill function phase]
                    :or {period 1.
                         start 0.
                         fill 0.5
                         phase 0.
                         duration 10.
                         amplitude 1.}}]
  (let [stop (+ start duration)
        base-func (functions-cont function)]
    {:type :fancy
     :period (and (not (#{:jump} function)) period)
     :start start
     :stop stop
     :fun (fn [time]
            (if-not (<= start time stop) 0.0
                    (let [angle (/ (mod (- time (* period phase)) period) period)
                          result (base-func {:angle angle
                                             :fill fill
                                             :time time})]
                      (* result amplitude))))}))

(defn discrete->pretendfancy [{:keys [sampling start stop values]}]
  {:type :fancy
   :period nil
   :start start
   :stop stop
   :fun (fn [time]
          (let [sample (int (/ (- time start) sampling))]
            (get values sample 0.0)))})

(defn want-fancy [{:keys [type] :as o}]
  ;; TODO: rewrite with mutli-methods?
  (condp = type
    :fancy o
    :discrete (discrete->pretendfancy o)
    (spec->fancy o)))

(defn gcd [a b]
  (let [scale (* (max a b) 1e-10)
        zero? #(> scale (abs %))]
    (loop [a a b b]
      (cond (> a b) (recur b a)
            (zero? a) b
            :else (recur (rem b a) a)))))

(defn lcm [a b] (and a b (/ (* a b) (gcd a b))))

(defn D "returns fancy with values after applying operator to each set"
  [operator & xs]
  (let [xs (mapv want-fancy xs)
        start (apply min (mapv :start xs))
        stop (apply max (mapv :stop xs))
        fns (mapv :fun xs)
        period (->> xs (mapv :period) (reduce lcm))]
    {:type :fancy
     :start start
     :stop stop
     :period period
     :fun (fn [x] (apply operator (mapv #(% x) fns)))}))

(defn impulse [& {:keys [sampling start duration ns A] :or
                  {sampling 1/1000 start 0 duration 1000 A 1}}]
  {:type :discrete :sampling sampling :start start :stop (+ start (* sampling duration))
   :values (vec (concat (repeat (- ns start) 0)
                        [A]
                        (repeat (- duration 1 ns) 0)))})

(defn impulse-noise [& {:keys [sampling start duration p A] :or
                        {sampling 1/1000 start 0 duration 1000 A 1}}]
  {:type :discrete :sampling sampling :start start :stop (+ start (* sampling duration))
   :values (vec (repeatedly duration (fn [] (if (> (rand) p) 0 A))))})

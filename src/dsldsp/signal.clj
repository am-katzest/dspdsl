(ns dsldsp.signal
  (:import [cern.jet.random.tdouble Normal])
  (:require [clojure.math]
            [better-cond.core :as b]))

(comment
  ;; signal examples, this is all docs you'll get ^w^
  '{:type :discrete :sampling 1/1000 :start 0 :duration 2 :values [0 0.1]}
  '{:type :fancy
    :start 0
    :stop 1
    :fun (fn [time] 0.0)}
  '{:function :square
    :amplitude 1
    :period 1.
    :start 0
    :duration 0.1
    :fill 0.1})

(defn get-format
  "in what format is signal stored?"
  [x]
  (b/cond (string? x) :file
          :let [t (:type x)]
          (nil? t) :spec
          t))

(defmulti fancy "get graph in fancy format" get-format)
(defmulti discrete "get graph in discrete format" get-format)

(defmulti proper-signal get-format)
(defmethod proper-signal :default [x] x)
(defmethod proper-signal :file [x] (discrete x))
(defmethod proper-signal :spec [x] (fancy x))

(def ^:dynamic sampling-frequency 1/1000)

(defn max0 [x] (max x 0))

(defn min0 [x] (min x 0))

(defn clamp ([m] (clamp m (- m)))
  ([m -m] (fn [x] (max -m (min m x)))))

(defn div0 [& xs]
  (try (apply / xs)
       (catch java.lang.ArithmeticException _ 0)))

(def functions-cont
  ;; they are all normalized
  (let [sin (fn [{:keys [angle]}] (Math/sin (* angle 2.0 Math/PI)))
        triangle (fn [{:keys [angle fill]}] (if (> fill angle)
                                              (div0 angle fill)
                                              (div0 (- 1 angle) (- 1 fill))))]
    {:noise (fn [_] (- 1 (* 2 (rand))))
     :noise-gauss (fn [_] (Normal/staticNextDouble 1.0 1.0))
     :sin sin
     :const (fn [_] 1.)
     :triangle  triangle
     :square (fn [{:keys [angle fill]}] (if (> fill angle) 1 0))
     :square-sym (fn [{:keys [angle fill]}] (if (> fill angle) 1 -1))
     :sin-half (comp max0 sin)
     :sin-double (comp abs sin)
     :jump (fn [{:keys [time fill]}] (if (> time fill) 1 0))}))

(defn- spec->fancy [{:keys [amplitude period start duration fill function phase]
                     :or {period 1.
                          start 0.
                          fill 0.5
                          phase 0.
                          duration 5.
                          amplitude 1.}}]
  (let [stop (+ start duration)
        base-func (functions-cont function)]
    {:type :fancy
     :period (if (#{:jump} function) 0.0 period)
     :start start
     :stop stop
     :fun (fn [time]
            (if-not (<= start time stop) 0.0
                    (let [angle (div0 (mod (- time (* period phase)) period) period)
                          result (base-func {:angle angle
                                             :fill fill
                                             :time time})]
                      (* result amplitude))))}))

(defn- discrete->pretendfancy [{:keys [sampling start duration values period]}]
  {:type :fancy
   :period period
   :start (* start sampling)
   :stop (* (+ start duration) sampling)
   :fun (fn [time]
          (let [sample (int (- (/ time sampling) start))]
            (get values sample 0.0)))})

(defn- fancy->discrete [x &
                        {:keys [sampling]
                         :or {sampling sampling-frequency}}]
  (let [{:keys [fun period start stop]} (fancy x)
        values (->> (range start stop sampling)
                    (mapv fun))]
    {:type :discrete
     :period period
     :sampling sampling
     :duration (count values)
     :start (int (/ start sampling))
     :values values}))

(defn- gcd [a b]
  (let [scale (* (max a b) 1e-10)
        zero? #(> scale (abs %))]
    (loop [a a b b]
      (cond (> a b) (recur b a)
            (zero? a) b
            :else (recur (rem b a) a)))))

(defn- lcm [a b] (if (and (pos? a) (pos? b))
                   (/ (* a b) (gcd a b))
                   0.0))

(defn fop "applies operator to values of signals at each point of time"
  [operator & xs]
  (let [xs (mapv fancy xs)
        start (apply min (mapv :start xs))
        stop (apply max (mapv :stop xs))
        fns (mapv :fun xs)
        period (->> xs (mapv :period) (reduce lcm))]
    {:type :fancy
     :start start
     :stop stop
     :period period
     :fun (fn [x] (apply operator (mapv #(% x) fns)))}))

(defn dop "applies operator to values of signals at each point of time, assumes that sampling is exactly the same"
  [operator & xs]
  (let [xs (mapv discrete xs)
        start (apply min (mapv :start xs))
        end (apply max (mapv #(+ (:start %) (:duration %)) xs))
        val (fn [{:keys [start values]} time]
              (get values (- time start) 0.))
        values (mapv
                (fn [time]
                  (apply operator (mapv #(val % time) xs)))
                (range start end))]
    {:type :discrete
     :sampling (:sampling (first xs))
     :start start
     :duration (count values)
     :values values}))

(defmulti tshift "shift signal x by t seconds" (fn [x t] (get-format x)))
(defmethod tshift :default [x t] (tshift (proper-signal x) t))

(defmethod tshift :discrete [{:keys [start sampling] :as x} t]
  (assoc x :start (/  (+ t (* start sampling)) sampling)))

(defmethod tshift :fancy [{:keys [start stop fun] :as x} t]
  (assoc x :start (+ start t)
         :stop (+ stop t)
         :fun (fn [a] (fun (- a t)))))

(defn impulse [& {:keys [sampling start duration ns A] :or
                  {sampling 1/1000 start 0 duration 1000 A 1}}]
  {:type :discrete :sampling sampling :start start :duration duration :period 0.
   :values (vec (concat (repeat (dec ns) 0)
                        [A]
                        (repeat (- duration 1 ns) 0)))})

(defn impulse-noise [& {:keys [sampling start duration p A] :or
                        {sampling 1/1000 start 0 duration 1000 A 1}}]
  {:type :discrete :sampling sampling :start start :duration duration :period 0.
   :values (vec (repeatedly duration (fn [] (if (> (rand) p) 0 A))))})

(defn make-complex [a b]
  (let [a (discrete a)
        b (discrete b)]
    ;; very safe, etc
    (assoc a :imaginary (:values b))))

;; conversion magic
(defmethod discrete :default [x] (discrete (proper-signal x)))
(defmethod discrete :discrete [x] x)
(defmethod discrete :fancy [x] (fancy->discrete x))
(defmethod fancy :default [x] (fancy (proper-signal x)))
(defmethod fancy :discrete [x] (discrete->pretendfancy x))
(defmethod fancy :fancy [x] x)
(defmethod fancy :spec [x] (spec->fancy x))

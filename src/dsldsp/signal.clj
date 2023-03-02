(ns dsldsp.signal
  (:import [cern.jet.random.tdouble Normal])
  (:require [clojure.math]
            [better-cond.core :as b]))

(comment
  ;; signal examples, this is all docs you'll get ^w^
  '{:type :discrete :sampling (/ 1000) :start 0 :duration 100 :values [0 0.1 0.3 ...]}
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

(defn graph-format [x]
  (b/cond (string? x) :file
          :let [t (:type x)]
          (nil? t) :spec
          t))

;┏━┓┏┓╻┏━┓╻  ┏━┓┏━╸
;┣━┫┃┗┫┣━┫┃  ┃ ┃┃╺┓
;╹ ╹╹ ╹╹ ╹┗━╸┗━┛┗━┛
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

(defn spec->fancy [{:keys [amplitude period start duration fill function phase]
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

(defn discrete->pretendfancy [{:keys [sampling start duration values period]}]
  {:type :fancy
   :period period
   :start (* start sampling)
   :stop (* (+ start duration) sampling)
   :fun (fn [time]
          (let [sample (int (/ (- time start) sampling))]
            (get values sample 0.0)))})

(defmulti want-fancy graph-format)
(defmulti want-discrete graph-format)
;; (defn want-discrete [{:keys [type] :as o}]
;;   (condp = type
;;     :fancy (fancy->discrete o)
;;     :discrete o
;;     (recur (want-fancy o))))

;; (defn want-fancy [{:keys [type] :as o}]
;;   (condp = type
;;     :fancy o
;;     :discrete (discrete->pretendfancy o)
;;     (spec->fancy o)))

(defn fancy->discrete [x &
                       {:keys [sampling]
                        :or {sampling sampling-frequency}}]
  (let [{:keys [fun period start stop]} (want-fancy x)
        values (->> (range start stop sampling)
                    (mapv fun))]
    {:type :discrete
     :period period
     :sampling sampling
     :duration (count values)
     :start (int (/ start sampling))
     :values values}))

(defn gcd [a b]
  (let [scale (* (max a b) 1e-10)
        zero? #(> scale (abs %))]
    (loop [a a b b]
      (cond (> a b) (recur b a)
            (zero? a) b
            :else (recur (rem b a) a)))))

(defn lcm [a b] (if (and (pos? a) (pos? b))
                  (/ (* a b) (gcd a b))
                  0.0))

(defn fop "returns fancy with values after applying operator to each set (coerces to fancy)"
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

(defn dop "returns discrete with values after applying operator to each set (coerces to discrete) assumes that sampling is exactly the same"
  [operator & xs]
  (let [xs (mapv want-discrete xs)
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

;╺┳┓╻┏━┓┏━╸┏━┓┏━╸╺┳╸┏━╸
; ┃┃┃┗━┓┃  ┣┳┛┣╸  ┃ ┣╸
;╺┻┛╹┗━┛┗━╸╹┗╸┗━╸ ╹ ┗━╸

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

;┏━╸┏━┓┏┳┓┏━┓╻  ┏━╸╻ ╻
;┃  ┃ ┃┃┃┃┣━┛┃  ┣╸ ┏╋┛
;┗━╸┗━┛╹ ╹╹  ┗━╸┗━╸╹ ╹

(defn make-complex [a b]
  (let [a (want-discrete a)
        b (want-discrete b)]
    ;; very safe, etc
    (assoc a :imaginary (:values b))))

(defmethod want-fancy :default [x] (want-fancy (want-discrete x)))
(defmethod want-discrete :default [x] (want-discrete (want-fancy x)))

(defmethod want-discrete :discrete [x] x)
(defmethod want-fancy :fancy [x] x)

(defmethod want-fancy :discrete [x] (discrete->pretendfancy x))
(defmethod want-discrete :fancy [x] (fancy->discrete x))

(defmethod want-fancy :spec [x] (spec->fancy x))

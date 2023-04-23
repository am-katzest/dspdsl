(ns dsldsp.signal
  (:import [cern.jet.random.tdouble Normal])
  (:require [clojure.math]
            [complex.core :as c]
            [dsldsp.convolution :as conv]
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
          (or (vector? x) (seq? x)) :filter
          :let [t (:type x)]
          (nil? t) :spec
          t))

(defmulti fancy "get graph in fancy format" (fn [x] (get-format x)))
(defmulti discrete "get graph in discrete format" (fn [x] (get-format x)))

(defmulti proper-signal get-format)
(defmethod proper-signal :default [x] x)
(defmethod proper-signal :file [x] (discrete x))
(defmethod proper-signal :filter [x] (discrete x))
(defmethod proper-signal :spec [x] (fancy x))

(def ^:dynamic sampling-period 1/1000)

(defn max0 [x] (max x 0))

(defn min0 [x] (min x 0))

(defn clamp ([m] (clamp m (- m)))
  ([m -m] (fn [x] (max -m (min m x)))))

(defn kwant [l]
  (fn [x] (* (int (/ ((if (pos? x) + -) x (/ l 2)) l)) l)))

(defn div0 [& xs]
  (try (apply / xs)
       (catch java.lang.ArithmeticException _ 0)))

(defn rename-keys [x m]
  (->> m
       (reduce
        (fn [x [name set]]
          (map (fn [[k v]] [(if (set k) name k) v])
               x)) x)
       (into {})))

(def functions-cont
  ;; they are all normalized
  (let [sin (fn [{:keys [angle]}] (Math/sin (* angle 2.0 Math/PI)))
        triangle (fn [{:keys [angle fill]}] (if (> fill angle)
                                              (div0 angle fill)
                                              (div0 (- 1 angle) (- 1 fill))))]
    {:noise (fn [_] (- 1 (* 2 (rand))))
     :noise-gauss (fn [_] (Normal/staticNextDouble 0.0 1.0))
     :noise-impulse (fn [{:keys [fill]}] (if (< fill (rand)) 0.0 1.0))
     :sin sin
     :const (fn [_] 1.)
     :triangle  triangle
     :square (fn [{:keys [angle fill]}] (if (> fill angle) 1 0))
     :square-sym (fn [{:keys [angle fill]}] (if (> fill angle) 1 -1))
     :sin-half (comp max0 sin)
     :sin-double (comp abs sin)
     :jump (fn [{:keys [time fill]}] (if (> time fill) 1 0))}))

(def spec-shorthands {:amplitude #{:A :a :amp}
                      :duration #{:d :l :len}
                      :start #{:s}
                      :end #{:e}
                      :function #{:f :fn :fun}
                      :period #{:p :per}
                      :phase #{:ph}})

(defn- spec->fancy [spec]
  (let [{:keys [amplitude period start duration fill function phase end spread]
         :or {period 1.
              start 0.
              fill 0.5
              phase 0.
              spread false
              duration 5.
              amplitude 1.}} (rename-keys spec spec-shorthands)
        stop (or end (+ start duration))
        base-func (functions-cont function)]
    {:type :fancy
     :period (if (#{:jump} function) 0.0 period)
     :start start
     :stop stop
     :fun (fn [time]
            (if-not (or spread (<= start time stop)) 0.0
                    (let [angle (div0 (mod (- time (* period phase)) period) period)
                          result (base-func {:angle angle
                                             :fill fill
                                             :time time})]
                      (* result amplitude))))}))

(defn d->f-meta [{:keys [sampling start duration period]} fun]
  {:type :fancy
   :period period
   :start (* start sampling)
   :stop (* (+ start duration) sampling)
   :fun fun})

(defn rzędu-zerowego [{:keys [sampling start values] :as x}]
  (d->f-meta x (fn [time]
                 (let [sample (int (- (/ time sampling) start))]
                   (get values sample 0.0)))))

(defn lin-comb [a b c]
  (+ (* a c)
     (* b (- 1 c))))

(defn rzędu-pierwszego [{:keys [sampling start values] :as x}]
  (d->f-meta x (fn [time]
                 (let [pos (- (/ time sampling) start)
                       n (int pos)
                       p (- pos n)]
                   (lin-comb (get values (inc n) 0.0)
                             (get values n 0.0)
                             p)))))

(defn sinc [x]
  (if (zero? x) 1.
      (/ (Math/sin (* Math/PI x)) x Math/PI)))

(defn sinc-1 [x n]
  (let [{:keys [sampling start values] :as x} (discrete x)
        Ts sampling]
    (d->f-meta x (fn [t]
                   (let [our-sample (int (/ t sampling))
                         samples-to-use (range (- our-sample n -1) (+ our-sample n 1))]
                     (->> (for [n samples-to-use]
                            (* (get values (- n start) 0.0) (sinc (- (/ t Ts) n))))
                          (reduce +)))))))
(defn sinc-2 [x n]
  (let [{:keys [sampling start values] :as x} (discrete x)
        Ts sampling
        vals (double-array values)
        end (+ start  (count values))]
    (d->f-meta x (fn [t]
                   (let [our-sample (int (/ t sampling))
                         s (max start (- our-sample n -1))
                         e (min end  (+ our-sample n 1))]
                     (loop [n s acc 0.0]
                       (if (= n e) acc
                           (recur (inc n)
                                  (+ acc (* (aget vals (- n start)) (sinc (- (/ t Ts) n))))))))))))

(defn fancy->discrete [x &
                       {:keys [sampling]
                        :or {sampling sampling-period}}]
  (let [{:keys [fun complex period start stop]} (fancy x)
        values (->> (range start stop sampling)
                    (mapv fun))
        base {:type :discrete
              :period period
              :sampling sampling
              :duration (count values)
              :start (int (/ start sampling))}]
    (if complex
      (assoc base
             :values (mapv c/real-part values)
             :imaginary (mapv c/imaginary-part values))
      (assoc base :values values))))

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

(defn combine-meta-fancy
  ([a b]
   (assoc a
          :start (min (:start a) (:start b))
          :stop (max (:stop a) (:stop b))
          :period (lcm (:period a) (:period b))
          :type :fancy))
  ([a]
   (reduce combine-meta-fancy a)))

(defn fop "applies operator to values of signals at each point of time"
  [operator & xs]
  (let [xs (mapv fancy xs)
        fns (mapv :fun xs)]
    (assoc (combine-meta-fancy xs)
           :fun (fn [x] (apply operator (mapv #(% x) fns))))))

(defmulti get-sampling get-format)
(defmethod get-sampling :default [x] nil)
(defmethod get-sampling :discrete [x] (:sampling x))
(defmethod get-sampling :file [x] (:sampling (discrete x)))

(defn fix-frequency-or-throw [xs]
  (b/cond
    :let [s (->> xs (map proper-signal) (keep get-sampling) (map float) sort dedupe)]
    (not (#{0 1} (count s))) (throw (ex-info "different sampling frequencies!" {:freq s}))
    :let [sf (or (first s) sampling-period)]
    (binding [sampling-period sf] (mapv discrete xs))))

(defn dop "applies operator to (complex) values of signals at each point of time"
  [operator & xs]
  (b/cond
    :let [xs (fix-frequency-or-throw xs)
          start (apply min (mapv :start xs))
          end (apply max (mapv #(+ (:start %) (:duration %)) xs))
          period (reduce lcm (mapv :period xs))
          get-complex (fn [{:keys [start values imaginary]} time]
                        (c/complex (get values (- time start) 0.)
                                   (get imaginary (- time start) 0.)))
          get-plain (fn [{:keys [start values]} time]
                      (get values (- time start) 0.))
          complex (seq (filter :imaginary xs))
          val (if complex get-complex get-plain)
          values (mapv
                  (fn [time]
                    (apply operator (mapv #(val % time) xs)))
                  (range start end))
          answer {:type :discrete
                  :sampling (:sampling (first xs))
                  :start start
                  :period  period
                  :duration (count values)}]
    complex (assoc answer
                   :imaginary (mapv c/imaginary-part values)
                   :values (mapv c/real-part values))
    :else (assoc answer
                 :values values)))

(defmulti tshift "shift signal x by t seconds" (fn [x t] (get-format x)))
(defmethod tshift :default [x t] (tshift (proper-signal x) t))

(defmethod tshift :discrete [{:keys [start sampling] :as x} t]
  (assoc x :start (/  (+ t (* start sampling)) sampling)))

(defmethod tshift :fancy [{:keys [start stop fun] :as x} t]
  (assoc x :start (+ start t)
         :stop (+ stop t)
         :fun (fn [a] (fun (- a t)))))

(defn make-it-add-up-to-one [x]
  (let [sum (reduce + x)
        factor (/ sum)
        fixer (fn [x] (* factor x))]
    (mapv fixer x)))

(defn make-filter [M K]
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

(defn hamming [M]
  (mapv (fn [n] (+ 0.53836
                   (* -0.46164 (Math/cos (* 2 Math/PI n (/ M)))))) (range M)))

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

(defn middle-pass [window]
  (map * window (cycle [0 2 0 -2])))

(defn lower-pass [window]
  (map * window (cycle [1 -1])))

(middle-pass [1 1 1 1 1 1 1 1 1 1])

(defn impulse [& {:keys [sampling ns A len] :or
                  {sampling sampling-period ns 0 A 1 len 1}}]
  {:type :discrete :sampling sampling :start ns
   :duration len :period 0. :values (vec (repeat len A))})

(defn pad-discrete [[a b]]
  [(dop (fn [a _] a) a b)
   (dop (fn [_ b] b) a b)])

(defn make-complex [a b]
  (let [a (proper-signal a)
        b (proper-signal b)]
    (if (or (= :discrete (:type a)) (= :discrete (:type b)))
      ;; signal is discrete
      ;; TODO add padding
      (let [[a b] (pad-discrete (fix-frequency-or-throw [a b]))]
        (assoc a :imaginary (:values b)))
      ;; signal is fancy
      (let [a (fancy a)
            b (fancy b)
            fa (:fun a)
            fb (:fun b)]
        (assoc (combine-meta-fancy a b)
               :fun #(c/complex (fa %) (fb %))
               :complex true)))))

(def ^:dynamic *interpolate* rzędu-pierwszego)

;; conversion magic
(defmethod discrete :default [x] (discrete (proper-signal x)))
(defmethod discrete :discrete [x] x)
(defmethod discrete :fancy [x] (fancy->discrete x))
(defmethod discrete :filter [x] (make-window x))
(defmethod fancy :default [x] (fancy (proper-signal x)))
(defmethod fancy :discrete [x] (*interpolate* x))
(defmethod fancy :fancy [x] x)
(defmethod fancy :spec [x] (spec->fancy x))

(defn complex-samples [x]
  (let [{:keys [values complex]} (discrete x)]
    (if complex
      (map c/complex values complex)
      (map c/complex values))))

(defn convolute [a b]
  (let [[a b] (fix-frequency-or-throw [a b])
        vals (conv/convolute (:values a) (:values b))
        start (+ (:start a) (:start b))]
    (assoc a :start start :values vals :duration (count vals))))

(defn normalize [x]
  (let [x (discrete x)
        vals (:values x)
        min (apply min vals)
        max (apply max vals)
        span (- max min)
        fixer (fn [x] (/ (- x min) span))]
    (dop fixer x)))

(defn  correlate [a b]
  (let [[a b] (pad-discrete  (fix-frequency-or-throw [a b]))]
    (convolute a b)))

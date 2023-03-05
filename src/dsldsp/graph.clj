(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as is]
            [incanter.io :as io]
            [dsldsp.signal :as s]
            [better-cond.core :as b]))

(def ^:dynamic graph-samples 2000)
(def ^:dynamic hist-bins 20)

(defn- graph-fancy [x]
  (let [{:keys [start stop fun]} (s/fancy x)
        diff (- stop start)
        margin (* diff 0.05)]
    (i/view (c/function-plot fun
                             ;; (- start margin)
                             ;; (+ stop margin)
                             start stop
                             :step-size (/ diff graph-samples)))))

(defn- graph-discrete [{:keys [start duration values sampling imaginary]}]
  (let [x-vals (mapv #(* % sampling) (range start (+ start duration)))
        x (c/scatter-plot x-vals values)]
    (when imaginary
      (c/add-points x x-vals imaginary))
    (i/view x)))

(defn truncate [period sampling xs]
  (b/cond
    (or (nil? period) (zero? period)) xs
    :let [samples-per-period (/ period sampling)]
    (>= samples-per-period (count xs)) xs ;no way to fix it ¯\_(ツ)_/¯
    :let [to-drop (rem (count xs) samples-per-period)]
    (vec (drop-last to-drop xs))))

(defn stat [x]
  (let [{:keys [values imaginary sampling period]} (s/discrete x)
        calc (fn [xs] (let [xs (truncate period sampling xs)
                            time (/ (count xs)) ; TODO probably off by one
                            avg-by #(->> xs (map %) (reduce +) (* time))
                            avg (avg-by identity)
                            avg2 (avg-by #(* % %))]
                        {:średnia-bezwzględna (avg-by abs)
                         :średnia avg
                         :moc-średnia avg2
                         :wariancja (avg-by #(abs (- % avg)))
                         :wartość-skuteczna (Math/sqrt avg2)}))]

    (if imaginary
      {:real (calc values)
       :imag (calc imaginary)}
      (calc values))))

(defn unzip [xs]
  (let [c (count (first xs))]
    (for [x (range c)]
      (map #(nth % x) xs))))

(defn histogram [in]
  (let [{:keys [values sampling period imaginary]} (s/discrete in)
        ;; todo add an ability to crop it according to stated period
        allvalues (concat values imaginary)
        minv (apply min allvalues)
        maxv (apply max allvalues)
        diff (- maxv minv)
        bin-size (/ diff hist-bins)
        round #(* (Math/round (float (/ (+ % (/ bin-size 2)) bin-size))) bin-size)
        bin-values (fn [xs] (->>
                             xs
                             (truncate period sampling)
                             (group-by round)
                             (map (fn [[x vals]]
                                    (let [p (/ (count vals) (count xs))]

                                      [[(- x (* 0.99999999 bin-size))
                                        (- x (* 0.9999999 bin-size))
                                        (- x (* 0.0000001 bin-size))
                                        x]
                                       [0 p p 0]])))
                             sort
                             unzip
                             (map (partial apply concat))))
        [real-x real-y] (bin-values values)
        real (c/xy-plot real-x real-y :legend true :series-label :real)]
    (when imaginary
      (let [[imag-x imag-y] (bin-values imaginary)]
        (c/add-lines real imag-x imag-y :series-label :imag)))
    (i/view real)
    (stat in)))

(defmulti graph s/get-format)
(defmethod graph :default [x] (graph (s/proper-signal x)))
(defmethod graph :discrete [x] (graph-discrete x))
(defmethod graph :fancy [x] (graph-fancy x))

(defn show
  [x]
  (graph x)
  (stat x))

(defn showboth [x]
  (histogram x)
  (show x))

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

(defn histogram [in]
  (let [{:keys [values sampling period imaginary]} (s/discrete in)
        ;; todo add an ability to crop it according to stated period
        show (fn [xs title] (-> (truncate period sampling xs)
                                (c/histogram :nbins hist-bins :legend true :title title)
                                i/view))]
    (show values "real")
    (when imaginary
      (show imaginary "imag")))
  (stat in))

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

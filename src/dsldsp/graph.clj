(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as is]
            [incanter.io :as io]
            [dsldsp.signal :as s]))

(def ^:dynamic graph-samples 2000)
(def ^:dynamic hist-bins 20)

(defn- graph-analog [x]
  (let [{:keys [start stop fun]} (s/want-fancy x)
        diff (- stop start)
        margin (* diff 0.05)]
    (i/view (c/function-plot fun
                             ;; (- start margin)
                             ;; (+ stop margin)
                             start stop
                             :step-size (/ diff graph-samples)))))

(defn- graph-discrete [{:keys [start duration values sampling imaginary]}]
  (let [x-vals (mapv #(* % sampling) (range start duration))
        x (c/scatter-plot x-vals values)]
    (when imaginary
      (c/add-points x x-vals imaginary))
    (i/view x)))

(defn histogram [in]
  (let [{:keys [values imaginary]} (s/want-discrete in)
        ;; todo add an ability to crop it according to stated period
        x (c/histogram values :nbins hist-bins)]
    (when (imaginary)
      (c/add-histogram x imaginary :nbins hist-bins))
    (i/view x)))

(defn show
  [x]
  (if (= :discrete (:type x))
    (graph-discrete x)
    (graph-analog x))
  nil)
(defn showboth [x]
  (histogram x)
  (show x))

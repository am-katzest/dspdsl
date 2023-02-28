(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as is]
            [incanter.io :as io]
            [dsldsp.signal :as s]))

(def ^:dynamic graph-samples 2000)
(def ^:dynamic hist-bins 20)

(defn- graph-fancy [x]
  (let [{:keys [start stop fun]} (s/want-fancy x)
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

(defn histogram [in]
  (let [{:keys [values imaginary]} (s/want-discrete in)
        ;; todo add an ability to crop it according to stated period
        ]
    (i/view (c/histogram values :nbins hist-bins :legend true))
    (when imaginary
      (i/view (c/histogram imaginary :nbins hist-bins :legend true)))))

(defn show
  [x]
  (if (= :discrete (:type x))
    (graph-discrete x)
    (graph-fancy x))
  nil)

(defn showboth [x]
  (histogram x)
  (show x))

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

(defn- graph-discrete [{:keys [start duration values sampling]}]
  (i/view (c/scatter-plot (mapv #(* % sampling) (range start duration)) values)))

(defn histogram [x] (let [vals (:values (s/want-discrete x))
            ;; todo add an ability to crop it according to stated period
                          min (apply min vals)
                          max (apply max vals)
                          span (- max min)
                          bin-size (/ span hist-bins)
                          bin #(* bin-size (int (+ 0.5 (/ % bin-size)))) ;bin to smaller
                          fixedvals (->> vals
                                         (map bin))]
                      (i/view (c/histogram vals :nbins hist-bins))))

(defn show
  [x]
  (if (= :discrete (:type x))
    (graph-discrete x)
    (graph-analog x))
  nil)
(defn showboth [x]
  (histogram x)
  (show x))

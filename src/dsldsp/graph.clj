(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as is]
            [incanter.io :as io]
            [dsldsp.signal :as s]))

(def ^:dynamic graph-samples 2000)
(def ^:dynamic hist-samples 10)

(defn- graph-analog [x]
  (let [{:keys [start stop fun]} (s/want-fancy x)
        diff (- stop start)
        margin (* diff 0.05)]
    (i/view (c/function-plot fun
                             ;; (- start margin)
                             ;; (+ stop margin)
                             start stop
                             :step-size (/ diff graph-samples))))
  nil)

(defn- graph-discrete [{:keys [start duration values sampling]}]
  (i/view (c/scatter-plot (mapv #(* % sampling) (range start duration)) values))
  nil)

(defn show
  [x]
  (if (= type :discrete (:type x))
    (graph-discrete x)
    (graph-analog x)))

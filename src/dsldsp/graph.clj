(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as is]
            [incanter.io :as io]
            [dsldsp.signal :as s]))
(def ^:dynamic graph-samples 2000)
(def ^:dynamic hist-samples 10)
(defn- show-graph [start stop fun]
  (let
   [diff (- stop start)
    margin (* diff 0.05)]
    (i/view (c/function-plot fun
                             (- start margin)
                             (+ stop margin)
                             :step-size (/ diff graph-samples))))
  nil)

(defn show ([x]
            (let [{:keys [start stop fun]} (s/want-fancy x)]
              (show-graph start stop fun)))
  ([x start stop]
   (let [{:keys [fun]} (s/want-fancy x)]
     (show-graph start stop fun)))
  ([x _]
   (let [{:keys [fun period start]} (s/want-fancy x)]
     (show-graph  0 (* period 1.5) fun))))

(defn hist [x])

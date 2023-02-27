(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]))

(defn show [{:keys [start stop fun]}]
  (let [diff (- stop start)
        margin (* diff 0.05)]
    (i/view (c/function-plot fun (- start margin) (+ stop margin)))))

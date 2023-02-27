(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [dsldsp.signal :as s]))

(defn show [x]
  (let [{:keys [start stop fun]} (s/want-fancy x)
        diff (- stop start)
        margin (* diff 0.05)]
    (i/view (c/function-plot fun (- start margin) (+ stop margin))))
  nil)

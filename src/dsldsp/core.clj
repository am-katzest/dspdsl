(ns dsldsp.core
  (:require [dsldsp.signal :as s]
            [dsldsp.graph :as g]))

(g/show
 (s/fop +
        {:function :sin :amplitude 0.5}
        {:function :square-sym :period 2 :amplitude 0.3}
        ;; {:function :noise-gauss :amplitude 0.01}
        ))

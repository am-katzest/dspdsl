(ns dsldsp.core
  (:require [dsldsp.graph :as g]
            [dsldsp.signal :as s]))
(g/show (s/D+ {:function :sin}
              {:function :sin :amplitude 0.2 :period 0.1}
              {:function :sin :amplitude 0.1 :period 0.05 :phashe 0.01}
              {:function :square :period 2 :amplitude 2}))

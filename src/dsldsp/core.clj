(ns dsldsp.core
  (:require [dsldsp.graph :as g]
            [dsldsp.signal :as s]))

(binding [g/graph-samples 2000]
  (g/show (s/D + (s/impulse-noise :p 0.5 :duration 10000 :A 0.1)
               (s/impulse-noise  :sampling 1/10 :p 0.5 :duration 100 :A 1))))

(ns dsldsp.core
  (:require [dsldsp.graph :as g]
            [dsldsp.signal :as s]))

(g/show (s/discrete->pretendfancy (s/impulse :ns 500)))

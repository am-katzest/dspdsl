(ns dsldsp.core
  (:require [dsldsp.graph :as g]
            [dsldsp.signal :as s]))
(g/show (s/spec->fancy {:function :square :fill 0.5}))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

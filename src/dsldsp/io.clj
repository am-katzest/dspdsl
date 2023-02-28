(ns dsldsp.io
  (:require [gloss.core :as g]
            [gloss.io :as i]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)))

(def ex {:type :discrete :sampling (/ 1000) :start 0 :duration 10
         :values (into [] (concat (repeat 2 0)
                                  (repeat 6 0.5)
                                  (repeat 2 1)))})
(def b (g/compile-frame {:type :discrete
                         :start :int64
                         :duration :int64
                         :sampling :float64
                         :values (g/repeated :float64)}))
(defn write [f discrete]
  (with-open [fs (io/output-stream discrete)]
    (i/encode-to-stream b fs [f])))

(defn read [f]
  (with-open [fs (io/input-stream f)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy fs out)
    (i/decode b (ByteBuffer/wrap (.toByteArray out)))))

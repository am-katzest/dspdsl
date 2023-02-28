(ns dsldsp.io
  (:require [gloss.core :as g]
            [gloss.io :as i]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)))

(def binary-schema (g/compile-frame {:type :discrete
                                     :start :int64
                                     :duration :int64
                                     :sampling :float64
                                     :values (g/repeated :float64)}))

(defn write [f discrete]
  (with-open [fs (io/output-stream discrete)]
    (i/encode-to-stream binary-schema fs [f])))

(defn- file->bb [f]
  (with-open [fs (io/input-stream f)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy fs out)
    (.toByteArray out)))

(defn read [f]
  (->> f
       file->bb
       ByteBuffer/wrap
       (i/decode binary-schema)))

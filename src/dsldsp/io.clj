(ns dsldsp.io
  (:require [gloss.core :as g]
            [gloss.io :as i]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)))

(def raw {:type :discrete
          :start :int64
          :duration :int64
          :sampling :float64
          :values (g/repeated :float64)})

(g/defcodec is-complex (g/enum :byte :plain :complex))

(g/defcodec plain raw)

(g/defcodec complex (assoc raw :complex (g/repeated :float64)))

(g/defcodec binary-schema
  (g/header
   is-complex
   {:complex complex :plain plain}
   #(if (:complex %) :complex :plain)))

(defn write [f discrete]
  (with-open [fs (io/output-stream f)]
    (i/encode-to-stream binary-schema fs [discrete])))

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

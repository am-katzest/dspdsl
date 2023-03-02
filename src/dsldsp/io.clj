(ns dsldsp.io
  (:require [gloss.core :as g]
            [gloss.io :as i]
            [dsldsp.signal :as sig]
            [clojure.java.io :as io])
  (:import (java.nio ByteBuffer)))

;; format (wielce redundantny): (marnujemy 4-8 bajtów) (wszystko big-endian)
;;; 0: bajt typu 0 - plain 1 - complex
;;; 1-4 int32 długość (całkowita ilość próbek) (nasza)
;;; 5-12 float64, zadeklarowany okres (do ucinania niepełnych okresów)
;;; 13-20 float64, okres próbkowania
;;; 21-24 int32 offset (w próbkach) początku

;;; 25-28 int32, ilość próbek (formatu)
;; cała masa float64
;; i tutaj ewentualnie jeszcze raz to samo dla wartości urojonych

(def raw {:type :discrete
          :duration :int32
          :period :float64
          :sampling :float64
          :start :int32
          :values (g/repeated :float64)})

(g/defcodec is-complex (g/enum :byte :plain :complex))

(g/defcodec plain raw)

(g/defcodec complex (assoc raw :imaginary (g/repeated :float64)))

(g/defcodec binary-schema
  (g/header
   is-complex
   {:complex complex :plain plain}
   #(if (:imaginary %) :complex :plain)))

(defn write [f discrete]
  (with-open [fs (io/output-stream f)]
    (i/encode-to-stream binary-schema fs [(sig/discrete discrete)])))

(defn- file->bb [f]
  (with-open [fs (io/input-stream f)
              out (java.io.ByteArrayOutputStream.)]
    (io/copy fs out)
    (ByteBuffer/wrap (.toByteArray out))))

(defn read [f]
  (->> f
       file->bb
       (i/decode binary-schema)))

(defmethod sig/discrete :file [x] (read x))

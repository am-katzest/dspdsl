(ns dsldsp.graph
  (:require [incanter.core :as i]
            [incanter.charts :as c]
            [incanter.stats :as is]
            [incanter.io :as io]
            [dsldsp.signal :as s]
            [better-cond.core :as b]))

(def ^:dynamic graph-samples 2000)
(def ^:dynamic hist-bins 20)

(defn- graph-fancy [x]
  (let [{:keys [start stop fun]} (s/want-fancy x)
        diff (- stop start)
        margin (* diff 0.05)]
    (i/view (c/function-plot fun
                             ;; (- start margin)
                             ;; (+ stop margin)
                             start stop
                             :step-size (/ diff graph-samples)))))

(defn- graph-discrete [{:keys [start duration values sampling imaginary]}]
  (let [x-vals (mapv #(* % sampling) (range start (+ start duration)))
        x (c/scatter-plot x-vals values)]
    (when imaginary
      (c/add-points x x-vals imaginary))
    (i/view x)))

(defn truncate [period sampling xs]
  '(b/cond
     (and period (pos? period)) xs
     :let [samples-per-period (/ period sampling)]
     (>= samples-per-period period) xs ;no way to fix it ¯\_(ツ)_/¯
     :let [to-drop (rem (count xs) period)]
     (vec (drop-last to-drop xs))))

(defn histogram [in]
  (let [{:keys [values sampling period imaginary]} (s/want-discrete in)
        ;; todo add an ability to crop it according to stated period
        show #(-> (truncate period sampling %)
                  (c/histogram :nbins hist-bins :legend true)
                  i/view)]
    (show values)
    (when imaginary
      (show imaginary))))

(defn show
  [x]
  (if (= :discrete (:type x))
    (graph-discrete x)
    (graph-fancy x))
  nil)

(defn showboth [x]
  (histogram x)
  (show x))
;┏┓╻┏━┓┏┓╻   ┏━╸┏━┓┏━┓┏━┓╻ ╻╻ ╻   ╺┳╸╻ ╻╻┏┓╻┏━╸┏━┓
;┃┗┫┃ ┃┃┗┫╺━╸┃╺┓┣┳┛┣━┫┣━┛┣━┫┗┳┛    ┃ ┣━┫┃┃┗┫┃╺┓┗━┓
;╹ ╹┗━┛╹ ╹   ┗━┛╹┗╸╹ ╹╹  ╹ ╹ ╹     ╹ ╹ ╹╹╹ ╹┗━┛┗━┛

(defn stat [x])

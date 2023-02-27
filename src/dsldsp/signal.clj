(ns dsldsp.signal)

(comment
  ;; signal types:
  '{:type :discrete :sampling (/ 1000) :start 0 :values [0 0.1 0.3 ...]}
  '{:type :fancy
    :start 0
    :stop 1
    :fun (fn [time] 0.0)}
  '{:type :spec
    :amplitude 1
    :period 1
    :start 0                            ; does start mean "shift phashe???" ;TODO
    :duration 0.1
    :fill 0.1
    :function :square})

(def functions
  ;; they are all normalized
  (let [sin (fn [angle _] (Math/sin (* angle 2.0 Math/PI)))
        min0 #(min % 0)
        abs #(if (pos? %) % (- %))]
    {:noise (fn [_] (- 1 (* 2 (rand))))
     :sin sin
     :triangle (fn [angle fill] (if (> fill angle) angle 0))
     :square (fn [angle fill] (if (> fill angle) 1 0))
     :sin-half (comp min0 sin)
     :sin-double (comp abs sin)}))

(defn spec->fancy [{:keys [amplitude period start duration fill function phase]
                    :or {period 1.
                         start 0.
                         fill 0.5
                         phase 0.
                         duration 3.
                         amplitude 1.}}]
  (let [stop (+ start duration)
        base-func (functions function)]
    {:type :fancy
     :start start
     :stop stop
     :fun (fn [time]
            (if-not (<= start time stop) 0.0
                    (let [angle (/ (mod (- time (* period phase)) period) period)
                          result (base-func angle fill)]
                      (* result amplitude))))}))
(defn want-fancy [{:keys [type] :as o}]
  ;; rewrite with mutli-methods?
  (condp = type
    :fancy o
    :discrete nil
    ;; TODO
    (spec->fancy o)))

(comment
  ;; usage example
  ;; spec has sane defaults
  '(let [x (spec
            :period 1
            :duration 4
            :function :sin)
         y (with x :function :triangle)]
     (graph (D+ x y))))

;; todo: function `with` (with cfg :function square) -> (create  (update cfg :function square))
;; (defn discretize [:keys])
(defn fancy-op [operator]
  (fn me ([ar br]
          (let [a (want-fancy ar)
                b (want-fancy br)
                start (min (:start a) (:start b))
                stop (max (:stop a) (:stop b))
                fa (:fun a)
                fb (:fun b)]
            {:type :fancy
             :start start
             :stop stop
             :fun (fn [x] (operator (fa x) (fb x)))}))
    ([a b & c]
     (apply me (me a b) c))))
(def D+ (fancy-op +))

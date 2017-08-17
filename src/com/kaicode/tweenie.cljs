(ns com.kaicode.tweenie
  (:require [com.kaicode.tily :as util]))

(defn additional-distance [start-val end-val fraction]
  (let [delta (- end-val start-val)]
    (if (>= fraction 1)
      delta
      (* fraction delta))))

(defn ease-linear [start-val end-val duration t]
  (let [fraction-of-time (/ t duration)]
    (+ start-val (additional-distance start-val end-val fraction-of-time))))

(defn ease-in [start-val end-val duration t]
  (let [fraction-of-time (/ t duration)
        fraction-of-time (js/Math.pow fraction-of-time 5)]
    (+ start-val (additional-distance start-val end-val fraction-of-time))))

(defn ease-out [start-val end-val duration t]
  (let [fraction-of-time (/ t duration)
        fraction-of-time (- 1
                            (js/Math.pow (- 1 fraction-of-time) 5))]
    (+ start-val (additional-distance start-val end-val fraction-of-time))))

(defmulti tween-val (fn [config-map delta-time]
                      (let [start-val (:from config-map)
                            type-start-val (type start-val)]
                        (cond
                          (= type-start-val cljs.core/PersistentVector) (if (= (-> start-val first type)
                                                                               js/Number)
                                                                          :vector
                                                                          :matrix)
                          (= type-start-val js/Number) :number))))


(defmethod tween-val :number [config-map delta-time]
  (let [start-num (:from config-map)
        end-num (:to config-map)
        duration (:duration config-map)
        easing-fn (:easing-fn config-map)

        tweened-num (easing-fn start-num end-num duration delta-time)]
    tweened-num))

(defmethod tween-val :vector [config-map delta-time]
  (let [start-vector (:from config-map)
        end-vector (:to config-map)
        duration (:duration config-map)
        easing-fn (:easing-fn config-map)
        
        start-vector-with-index (util/with-index start-vector)
        tweened-vec (mapv (fn [[i start-val]]
                            (let [end-val (end-vector i)]
                              (easing-fn start-val end-val duration delta-time)))
                          start-vector-with-index)]
    tweened-vec))

(defmethod tween-val :matrix [config-map delta-time]
  (let [start-matrix (:from config-map)
        end-matrix (:to config-map)
        duration (:duration config-map)
        easing-fn (:easing-fn config-map)
        start-rows-with-index (util/with-index start-matrix)
        tweened-vector (mapv (fn [[i start-row]]
                               (let [start-row-with-index (util/with-index start-row)
                                     end-row (end-matrix i)]
                                 (mapv (fn [[j start-val]]
                                         (let [end-val (end-row j)]
                                           (easing-fn start-val end-val duration delta-time)))
                                       start-row-with-index)))
                             start-rows-with-index)]
    tweened-vector))

(defn tween [config-map]
  (let [duration (:duration config-map)
        on-update (:on-update config-map)
        start-time (atom nil)
        continue? (atom true)]
    (fn [clock-time]
      (when (nil? @start-time)
        (reset! start-time clock-time))
      (let [delta-time (- clock-time @start-time)
            tweened-val (tween-val config-map delta-time)]
        (if (<= delta-time duration)
          (on-update tweened-val)
          (reset! continue? false)))
      @continue?)))

(defn animate [tween-fn]
  ((fn animation-loop [clock-time]
     (let [continue? (tween-fn clock-time)]
       (when continue?
         (js/requestAnimationFrame animation-loop))))
   (js/performance.now)
   ))


;; (def t (tween {:from      1 :to 1000
;;                :duration  1000
;;                :easing-fn ease-out
;;                :on-update (fn [val]
;;                             (let [s (dom/by-id "category-Sandwiches")]
;;                               (layout/position s val 0))
;;                             )}))

;; (def t2 (tween {:from [0 0] :to [1000 200]
;;                 :duration 10000
;;                 :easing-fn ease-linear
;;                 :on-update (fn [[x y]]
;;                              (let [s (dom/by-id "category-Sandwiches")]
;;                                (layout/position s x y))
;;                              )}))

;; (def t3 (tween {:from matrix/identity-matrix :to (matrix/multiply
;;                                                   (layout/translate-x 1000)
;;                                                   (layout/translate-y 200)
;;                                                   (layout/scale 1.5)
;;                                                   )
;;                 :duration 10000
;;                 :easing-fn ease-out
;;                 :on-update (fn [m]
;;                              (let [s (dom/by-id "category-Sandwiches")]
;;                                (layout/set-transform-matrix! s m)
;;                                )
;;                              )}))



# Tweenie

A tweening library written in ClojureScript. Can be used for CSS animation or use cases where a range
of values needs to be tweened.


# Install 

    [com.kaicode/tweenie "0.1.1-SNAPSHOT"]


# Example

```Clojure
  (def single-value-tweening (tween {:from      1 :to 1000
                                     :duration  1000
                                     :easing-fn ease-out
                                     :on-update (fn [val]
                                                  (let [s (dom/by-id "category-Sandwiches")]
                                                    (layout/position s val 0))
                                                  )}))

  (def vector-tweening (tween {:from [0 0] :to [1000 200]
                               :duration 10000
                               :easing-fn ease-linear
                               :on-update (fn [[x y]]
                                            (let [s (dom/by-id "category-Sandwiches")]
                                              (layout/position s x y))
                                            )}))

  (def matrix-tweening (tween {:from matrix/identity-matrix :to (matrix/multiply
                                                                 (layout/translate-x 1000)
                                                                 (layout/translate-y 200)
                                                                 (layout/scale 1.5)
                                                                 )
                               :duration 10000
                               :easing-fn ease-out
                               :on-update (fn [m]
                                            (let [s (dom/by-id "category-Sandwiches")]
                                              (layout/set-transform-matrix! s m))
                                            )}))

```

(ns fall
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.geom.svg.core :as svg]))

;; Function to generate Bridget Riley's "Fall" artwork
(defn fall-artwork
  [width height num-lines base-amplitude base-frequency]
  (let [line-spacing (/ height num-lines)
        lines (map (fn [i]
                     (let [y (* i line-spacing)
                           amplitude-factor (+ 0.5 (* 0.5 (Math/sin (* m/PI (/ i num-lines)))))
                           frequency-factor (+ 0.8 (* 0.4 (Math/cos (* m/PI 0.5 (/ i num-lines)))))
                           amplitude (* base-amplitude amplitude-factor)
                           frequency (* base-frequency frequency-factor)
                           points (map (fn [x]
                                         (let [phase-shift (* 0.1 x)
                                               offset (* amplitude 
                                                        (Math/sin (+ (* frequency x) 
                                                                   (* 0.5 amplitude (Math/sin phase-shift)))))]
                                           [x (+ y offset)]))
                                       (range 0 width 5))]
                       (svg/polyline points)))
                   (range num-lines))]
    (svg/svg
     {:width width :height height}
     (svg/rect [0 0] width height {:fill "white"})
     (svg/group
      {:stroke "black"
       :stroke-width "1"
       :fill "none"}
      lines))))

(def fall-art
  (fall-artwork 300 300 50 10 0.05))

(->> fall-art
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "fall-artwork2.svg"))

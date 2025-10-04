(ns cataract3
  (:require
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.geom.svg.core :as svg]))

(defn cataract-3
  []
  (let [width 300
        height 300
        num-lines 75
        line-spacing (/ height num-lines)
        base-amplitude 5
        frequency 0.055
        phase-shift 0.45
        lines (map (fn [i]
                     (let [y (* i line-spacing)
                           amplitude (* base-amplitude (+ 0.8 (* 0.4 (Math/sin (* i 0.2)))))
                           stroke-width 1
                           points (map (fn [x]
                                        (let [offset (* amplitude 
                                                        (Math/sin (+ (* frequency x) 
                                                                     (* phase-shift i))))]
                                          [x (+ y offset)]))
                                      (range 0 width 2))]
                       (svg/polyline points {:stroke-width stroke-width})))
                   (range num-lines))]
    (svg/svg
     {:width width :height height}
     (svg/rect [0 0] width height {:fill "white"})
     (svg/group
      {:stroke "black"
       :fill "none"}
      lines))))

(->> (cataract-3)
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "cataract-3.svg"))

(ns main
  (:require
   [thi.ng.math.core :as m]
   [thi.ng.geom.circle :as c]
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.geom.svg.core :as svg]))

;; Function to create concentric shapes
(defn concentric-shapes
  [center base-width base-height offset num-shapes shape-fn]
  (let [shapes (map (fn [i]
                      (let [offset (* offset i)
                            new-width (+ base-width (* 2 offset))
                            new-height (+ base-height (* 2 offset))]
                        (shape-fn center new-width new-height)))
                    (range num-shapes))]
    (svg/svg
     {:width 300 :height 300}
     (svg/rect [0 0] 300 300 {:fill "white"})
     (svg/group
      {:stroke "black"
       :stroke-width "5"
       :fill "none"}
      shapes))))

;; Shape generator functions
(defn circle-shape [center width _height]
  (c/circle center (/ width 2)))

(defn capsule-shape [center width height]
  (let [[cx cy] center
        half-height (/ height 2)
        half-width (/ width 2)
        radius half-height
        top-left [(- cx half-width) (- cy half-height)]
        top-right [(+ cx half-width) (- cy half-height)]
        bottom-right [(+ cx half-width) (+ cy half-height)]
        bottom-left [(- cx half-width) (+ cy half-height)]]
    (svg/path
     [[:M top-left]
      [:L top-right]
      [:A [radius radius] 0 "0" "1" bottom-right]
      [:L bottom-left]
      [:A [radius radius] 0 "0" "1" top-left]
      [:Z]])))

(defn s-shape [center width height]
  (let [[cx cy] center
        half-width (/ width 2)
        half-height (/ height 2)
        
        ;; Define key points
        top-right [(+ cx half-width) (- cy half-height)]
        middle-left [(- cx half-width) cy]
        bottom-right [(+ cx half-width) (+ cy half-height)]
        middle-right [(+ cx half-width) cy]
        
        ;; Define control points for curves
        c1-1 [(+ cx half-width) (- cy (* 0.75 half-height))]  ; Control point 1: down from top-right
        c1-2 [(- cx half-width) (- cy (* 0.25 half-height))]  ; Control point 2: up from middle-left
        
        c2-1 [(- cx half-width) (+ cy (* 0.25 half-height))]   ; Control point 1: down from middle-left
        c2-2 [(+ cx half-width) (+ cy (* 0.75 half-height))]   ; Control point 2: up from bottom-right
        
        c3-1 [(+ cx half-width) (+ cy (* 0.25 half-height))]   ; Control point 1: up from bottom-right
        c3-2 [(+ cx half-width) (- cy (* 0.25 half-height))]   ; Control point 2: down from middle-right
        
        c4-1 [(+ cx half-width) (- cy (* 0.75 half-height))]  ; Control point 1: down from middle-right
        c4-2 [(+ cx half-width) (- cy (* 0.25 half-height))]  ; Control point 2: up from top-right
        ]
    (svg/path
     [[:M top-right]
      [:C c1-1 c1-2 middle-left]
      [:C c2-1 c2-2 bottom-right]
      [:C c3-1 c3-2 middle-right]
      [:C c4-1 c4-2 top-right]
      [:Z]])))

(defn star-shape [center width _height]
  (let [[cx cy] center
        outer-radius (/ width 2)
        inner-radius (/ outer-radius 2)
        points (for [i (range 10)]
                 (let [angle (* m/PI 2 (/ i 10))
                       r (if (even? i) outer-radius inner-radius)
                       x (+ cx (* r (Math/cos angle)))
                       y (+ cy (* r (Math/sin angle)))]
                   [x y]))]
    (svg/polygon points)))

(def concentric-stars
  (concentric-shapes [150 150] 100 100 25 15 star-shape))

(def concentric-circles
  (concentric-shapes [150 150] 20 20 10 25 circle-shape))

(def concentric-capsules
  (concentric-shapes [150 150] 100 50 10 10 capsule-shape))

(def concentric-s-shapes
  (concentric-shapes [150 150] 100 100 10 10 s-shape))

(->> concentric-circles            
     (adapt/all-as-svg)              
     (svg/serialize)                 
     (spit "c-circles.svg"))  
     
(->> concentric-capsules
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "c-capsules.svg"))

(->> concentric-s-shapes
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "c-s-shapes.svg"))

(->> concentric-stars
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "c-s-stars.svg"))

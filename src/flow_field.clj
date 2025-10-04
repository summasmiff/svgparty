(ns flow-field
  (:require
   [thi.ng.geom.svg.adapter :as adapt]
   [thi.ng.geom.svg.core :as svg]))

(defn flow-field
  []
  (let [width 1000
        height 1000
        resolution-abs 14  ; Grid resolution (should be 0.5% - 2% of grid size) (PLAY WITH)
        margin 0.9
        
        ; Calculate grid bounds
        left-x (int (* width (- margin)))
        right-x (int (* width (+ 1 margin)))
        top-y (int (* height (- margin)))
        bottom-y (int (* height (+ 1 margin)))
        
        ; Calculate grid dimensions (ensure integers)
        num-columns (quot (- right-x left-x) resolution-abs)
        num-rows (quot (- bottom-y top-y) resolution-abs)
        
        ; Create smooth variations
        noise (fn [x y scale]
                (let [x1 (* x scale)
                      y1 (* y scale)]
                  (+ (Math/sin x1)
                     (Math/sin (* y1 1.3))
                     (Math/sin (* (+ x1 y1) 0.7))
                     (Math/sin (* x1 y1 0.01)))))
        
        ; Initialize grid with angles using noise
        grid (for [row (range num-rows)]
               (for [col (range num-columns)]
                 (* Math/PI 2 
                    (mod (+ 0.5 (* 0.5 (noise col row 0.05))) 1.0))))
        
        ; Function to get angle at a specific point
        get-angle (fn [x y]
                   (let [col (quot (- x left-x) resolution-abs)
                         row (quot (- y top-y) resolution-abs)]
                     (if (and (>= col 0) (< col num-columns)
                              (>= row 0) (< row num-rows))
                       (nth (nth grid row) col)
                       0)))  ; Default angle if out of bounds
        
        ; Function to generate a curve following the flow field
        generate-curve (fn [start-x start-y]
                        (loop [x start-x
                               y start-y
                               steps 200  ; Number of steps per curve
                               points [[start-x start-y]]]
                          (if (<= steps 0)
                            points
                            (let [angle (get-angle x y)
                                  new-x (+ x (* 2.8 (Math/cos angle)))  ; Step length (PLAY WITH)
                                  new-y (+ y (* 2.8 (Math/sin angle)))]
                              (recur new-x new-y (dec steps) (conj points [new-x new-y]))))))
        
        ; Generate starting points with more randomness
        starting-points (let [spacing 30  ; Base spacing (PLAY WITH)
                              cols (quot width spacing)
                              rows (quot height spacing)]
                          (for [row (range rows)
                                col (range cols)
                                :let [; Add significant randomness to position
                                      x (+ (* col spacing) 
                                           (rand-int spacing)  ; Random offset within cell
                                           (- (rand-int 40) 20))  ; Additional random offset
                                      y (+ (* row spacing) 
                                           (rand-int spacing)
                                           (- (rand-int 40) 20))]
                                :when (and (> x 20) (< x (- width 20))
                                           (> y 20) (< y (- height 20)))]
                            [x y]))
        
        ; Generate curves from starting points
        curves (map (fn [[start-x start-y]]
                      (generate-curve start-x start-y))
                    starting-points)
        
        ; Create SVG elements from curves
        svg-elements (map (fn [points]
                           (svg/polyline points {:stroke "black" :fill "none" :stroke-width "1"}))
                         curves)]
    
    ; Create SVG with white background and black flow lines
    (svg/svg
     {:width width :height height}
     (svg/rect [0 0] width height {:fill "white"})
     (apply svg/group
            {:stroke "black"
             :fill "none"}
            svg-elements))))

; Generate and save the SVG
(->> (flow-field)
     (adapt/all-as-svg)
     (svg/serialize)
     (spit "flow-field.svg"))

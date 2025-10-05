(ns noise-visualization
  (:require
   [hiccup.core :as hiccup]
   [noise-gradient-grid :as ng]))

(defn visualize-noise-grid
  "Visualizes the noise and gradient grid as an SVG with grayscale squares."
  ([width height resolution]
   (visualize-noise-grid width height resolution 0.05 0.3 0.7))
  ([width height resolution noise-scale gradient-power noise-weight]
   ; Generate the grid using the noise-gradient-grid namespace
   (let [[grid _max-points] (ng/create-noise-grid width height resolution 
                                                noise-scale gradient-power noise-weight)
         
         ; Prepare grid data for visualization
         viz-data (ng/visualize-grid grid)
         
         ; Create SVG rectangles for each grid cell
         grid-rects (for [{:keys [row col value color]} viz-data]
                     (let [x (* col resolution)
                           y (* row resolution)]
                       [:rect {:x x :y y 
                               :width resolution :height resolution 
                               :fill color}]))
         
         ; Create SVG with all rectangles
         svg-content [:svg {:width width :height height
                            :xmlns "http://www.w3.org/2000/svg"
                            :xmlns:xlink "http://www.w3.org/1999/xlink"
                            :version "1.1"}
                      grid-rects]]
     
     svg-content)))

(defn save-visualization
  "Generates and saves the noise grid visualization to an SVG file."
  ([filename width height resolution]
   (save-visualization filename width height resolution 0.05 0.3 0.7))
  ([filename width height resolution noise-scale gradient-power noise-weight]
   (->> (visualize-noise-grid width height resolution 
                             noise-scale gradient-power noise-weight)
        (hiccup/html {:mode :xml})
        (spit filename))))

; Example usage:
(save-visualization "noise-grid.svg" 1000 1000 10)
(ns noise-gradient-grid
  "Namespace for generating a grid with noise and gradient effects.
   The grid has exactly two points at maximum value (1.0) with a gradient
   that decreases from these points, combined with prominent noise.")

(defn create-noise-grid
  "Creates a grid of values with noise and gradient effects.
   
   Parameters:
   - width: Width of the grid
   - height: Height of the grid
   - resolution: Size of each grid cell
   - noise-scale: Scale factor for noise generation
   - gradient-power: Controls the steepness of the gradient (lower = weaker gradient)
   - noise-weight: How much noise contributes to the final value (0.0-1.0)
   
   Returns:
   - A 2D vector of values between 0.0 and 1.0
   - The coordinates of the two maximum points as [[row1 col1] [row2 col2]]"
  ([width height resolution]
   (create-noise-grid width height resolution 0.05 0.3 0.7))
  ([width height resolution noise-scale gradient-power noise-weight]
   (let [; Calculate grid dimensions
         num-columns (quot width resolution)
         num-rows (quot height resolution)
         
         ; Create smooth variations
         noise (fn [x y scale]
                 (let [x1 (* x scale)
                       y1 (* y scale)]
                   (+ (Math/sin x1)
                      (Math/sin (* y1 1.3))
                      (Math/sin (* (+ x1 y1) 0.7))
                      (Math/sin (* x1 y1 0.01)))))
         
         ; Pick two distinct random points to be the maximum
         max-row1 (rand-int num-rows)
         max-col1 (rand-int num-columns)
         max-row2 (loop [r (rand-int num-rows)]
                    (if (and (= r max-row1) 
                             (= (rand-int num-columns) max-col1))
                      (recur (rand-int num-rows))
                      r))
         max-col2 (loop [c (rand-int num-columns)]
                    (if (and (= max-row2 max-row1) (= c max-col1))
                      (recur (rand-int num-columns))
                      c))
         
         ; Compute maximum possible distance in grid
         max-distance (Math/sqrt (+ (* num-columns num-columns) (* num-rows num-rows)))
         
         ; Compute a new grid with gradient from the two maximum points and noise
         gradient-grid (for [row (range num-rows)]
                         (for [col (range num-columns)]
                           (if (or (and (= row max-row1) (= col max-col1))
                                   (and (= row max-row2) (= col max-col2)))
                             1.0  ; Maximum points have the maximum value
                             (let [; Calculate distances from both maximum points
                                   dist1 (Math/sqrt (+ (* (- col max-col1) (- col max-col1))
                                                      (* (- row max-row1) (- row max-row1))))
                                   dist2 (Math/sqrt (+ (* (- col max-col2) (- col max-col2))
                                                      (* (- row max-row2) (- row max-row2))))
                                   ; Use the minimum distance to either maximum point
                                   min-dist (min dist1 dist2)
                                   ; Normalize distance to [0, 1] range
                                   normalized-dist (/ min-dist max-distance)
                                   ; Apply gradient - value decreases with distance
                                   gradient-val (Math/pow (- 1 normalized-dist) gradient-power)
                                   ; Add noise (scaled to [0, 1])
                                   noise-val (* 0.5 (+ 1.0 (noise col row noise-scale)))
                                   ; Combine gradient and noise
                                   combined-val (+ (* gradient-val (- 1.0 noise-weight))
                                                  (* noise-val noise-weight))]
                               (min 0.95 combined-val)))))]
     
     [gradient-grid [[max-row1 max-col1] [max-row2 max-col2]]])))

(defn get-value
  "Get the value at a specific position in the grid.
   
   Parameters:
   - grid: The 2D grid of values
   - row: Row index
   - col: Column index
   
   Returns:
   - The value at the specified position, or 0.0 if out of bounds"
  [grid row col]
  (if (and (>= row 0) (< row (count grid))
           (>= col 0) (< col (count (first grid))))
    (nth (nth grid row) col)
    0.0))

(defn get-max-points
  "Get the coordinates of the two maximum value points.
   
   Parameters:
   - max-points: The max points returned by create-noise-grid
   
   Returns:
   - A vector of two [row col] coordinates"
  [max-points]
  max-points)

(defn normalize-grid
  "Normalize all values in the grid to the range [0.0, 1.0].
   
   Parameters:
   - grid: The 2D grid of values
   
   Returns:
   - A new grid with normalized values"
  [grid]
  (let [all-values (flatten grid)
        min-val (apply min all-values)
        max-val (apply max all-values)
        range-val (- max-val min-val)]
    (if (zero? range-val)
      grid
      (for [row grid]
        (for [val row]
          (/ (- val min-val) range-val))))))

(defn visualize-grid
  "Convert the grid to a format suitable for visualization.
   
   Parameters:
   - grid: The 2D grid of values
   
   Returns:
   - A sequence of maps with :row, :col, :value, and :color"
  [grid]
  (for [row (range (count grid))
        col (range (count (first grid)))
        :let [value (get-value grid row col)
              grey-level (int (* 255 (- 1 value)))
              color (format "rgb(%d,%d,%d)" grey-level grey-level grey-level)]]
    {:row row :col col :value value :color color}))
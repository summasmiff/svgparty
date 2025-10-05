(ns noise-gradient-grid)

(defn create-noise-grid
  "Creates a grid of values with noise and gradient effects.
   
   Parameters:
   - width
   - height
   - resolution: Size of each grid cell
   - noise-scale: Scale factor for noise generation
   
   Returns:
   - A 2D vector of values between 0.0 and 1.0
   - The coordinates of the two maximum points as [[row1 col1] [row2 col2]]"
  ([width height resolution]
   (create-noise-grid width height resolution 0.05))
  ([width height resolution noise-scale]
   (let [; Calculate grid dimensions
         num-columns (quot width resolution)
         num-rows (quot height resolution)

         ; Generate permutation table for Perlin noise
         permutation (let [p (shuffle (range 256))]
                       (vec (concat p p)))

         ; Helper functions for Perlin noise
         fade (fn [t]
                (let [t3 (* t t t)]
                  (* t3 (+ (* 6 t t) (* -15 t) 10))))

         lerp (fn [t a b]
                (+ a (* t (- b a))))

         grad (fn [hash x y]
                (let [h (bit-and hash 3)
                      u (if (< h 2) x y)
                      v (if (< h 2) y x)
                      term1 (if (zero? (bit-and h 1)) u (- u))
                      term2 (if (zero? (bit-and h 2)) v (- v))]
                  (+ term1 term2)))

         ; Perlin noise function
         noise (fn [x y scale]
                 (let [x (* x scale)
                       y (* y scale)
                       X (int x)
                       Y (int y)
                       xf (- x X)
                       yf (- y Y)
                       u (fade xf)
                       v (fade yf)
                       a (nth permutation (bit-and X 255))
                       b (nth permutation (bit-and (inc X) 255))
                       aa (nth permutation (bit-and (+ a Y) 255))
                       ab (nth permutation (bit-and (+ a (inc Y)) 255))
                       ba (nth permutation (bit-and (+ b Y) 255))
                       bb (nth permutation (bit-and (+ b (inc Y)) 255))
                       g00 (grad aa xf yf)
                       g10 (grad ba (- xf 1) yf)
                       g01 (grad ab xf (- yf 1))
                       g11 (grad bb (- xf 1) (- yf 1))
                       x1 (lerp u g00 g10)
                       x2 (lerp u g01 g11)
                       y1 (lerp v x1 x2)]
                   (/ y1 2.0)))  ; Normalize to [-1, 1]

         ; Pick two distinct random points to be the maximum
         gradient-grid (for [row (range num-rows)]
                         (for [col (range num-columns)]
                           (let [noise-val (noise col row noise-scale)]
                             (min 1 noise-val))))]
                             
                             gradient-grid)))

(defn get-value [grid row col]
  (if (and (>= row 0) (< row (count grid))
           (>= col 0) (< col (count (first grid))))
    (nth (nth grid row) col)
    0.0))

(defn visualize-grid [grid]
  (for [row (range (count grid))
        col (range (count (first grid)))
        :let [value (get-value grid row col)
              grey-level (int (* 255 (- 1 value)))
              color (format "rgb(%d,%d,%d)" grey-level grey-level grey-level)]]
    {:row row :col col :value value :color color}))

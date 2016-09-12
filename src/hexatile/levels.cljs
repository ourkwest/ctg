(ns hexatile.levels
  (:require
    [hexatile.constants :as c]))



(defn apply-step [[x y rotation] [angle distance]]
  (let [new-rotation (+ rotation angle)
        new-x (+ x (* distance (Math/sin new-rotation)))
        new-y (+ y (* distance (Math/cos new-rotation)))]
    [new-x new-y new-rotation]))

(defn locate                                                ;; was 'path'
  "Walk some steps from a location."
  [location & steps]
  (if-let [step (first steps)]
    (recur (apply-step location step) (rest steps))
    location))

(def pads {3 c/tri-pad
           4 c/square-pad
           6 c/hex-pad
           8 c/oct-pad})

(def angles {3 c/TAU_3RD
             4 c/TAU_4TH
             6 c/TAU_6TH
             8 c/TAU_8TH})

(defn mk-shapes
  "Transforms level input data structure into a vector of shapes for a level."
  [location [sides neighbour-pairs & rest]]
  (let [my-pad (pads sides)
        new-shape (when (not= sides 0) [{:n sides :location (locate location [0 my-pad] [c/PI 0])}])
        my-angle (angles sides)
        neighbour-angles (iterate #(+ % my-angle) (+ c/PI my-angle))
        neighbours (partition 2 neighbour-pairs)
        neighbour-shapes
        (apply concat
               (for
                 [i (range (count neighbours))]
                 (mk-shapes (locate location [0 my-pad] [(nth neighbour-angles i) my-pad])
                            (nth neighbours i))))
        more-shapes (when rest (mk-shapes location rest))]
    (vec (concat new-shape neighbour-shapes more-shapes))))

(defn round-to
  "Round a double to the given precision (number of significant digits)"
  [precision d]
  (let [factor (Math/pow 10 precision)]
    (/ (Math/round (* d factor)) factor)))

(defn round-location [[x y r]]
  [(round-to 2 x) (round-to 2 y) (round-to 4 r)])

(defn round-shapes [shapes]
  (mapv #(update % :location round-location) shapes))

(defn translate-each [by-x by-y shape]
  (-> shape
      (update-in [:location 0] + by-x)
      (update-in [:location 1] + by-y)))

(defn translate [shapes by-x by-y]
  (mapv (partial translate-each by-x by-y) shapes))

(def padding (round-to 2 (* 1.1 c/oct-radius)))

(defn centre [shapes]
  (let [xs (map first (map :location shapes))
        ys (map second (map :location shapes))
        min-x (reduce min xs)
        min-y (reduce min ys)
        max-x (reduce max xs)
        max-y (reduce max ys)
        new-shapes (translate shapes (- padding min-x) (- padding min-y))]
    [new-shapes (+ (* 2 padding) (- max-x min-x)) (+ (* 2 padding) (- max-y min-y))]))

(defn add-blank-wires [channel-count shape]
  (assoc shape :wiring (vec (for [i (range channel-count)] []))))

(defn blank-wires [shapes channel-count]
  (mapv (partial add-blank-wires channel-count) shapes))

(defn endpoint-wiring [channel-count direction shape]
  (assoc shape :wiring (vec (for [i (range channel-count)]
                              (vec (for [j (range (:n shape))]
                                     (if (= direction 1)
                                       [j 9]
                                       [9 j])))))))


(defn add-endpoint-wiring [shapes shape-id channel-count direction]
  (update-in shapes [shape-id] (partial endpoint-wiring channel-count direction)))

(defn get-sides [shape]
  (let [{[x y r] :location n :n} shape
        shape-angle (angles n)
        radius (pads n)]
    (for [side-angle (take n (iterate #(+ % shape-angle) r))]
      (let [side-x (+ x (* radius (Math/sin side-angle)))
            side-y (+ y (* radius (Math/cos side-angle)))]
        [side-x side-y shape]))))

(defn close-enough [[x1 y1] [x2 y2]]
  (let [x-diff (- x2 x1)
        y-diff (- y2 y1)
        h2 (+ (* x-diff x-diff) (* y-diff y-diff))
        limit (/ c/shape-side-length 3)
        l2 (* limit limit)]
    (< h2 l2)))

(defn find-neighbours [shapes [x y shape]]
  (some identity (for [i (range (count shapes))]
                   (cond
                     (= shape (nth shapes i)) nil
                     (some (partial close-enough [x y]) (get-sides (nth shapes i))) i))))

(defn add-neighbours [shapes]
  (mapv #(assoc % :neighbours (mapv (partial find-neighbours shapes) (get-sides %))) shapes))

(defn randomise-rotations [shapes]
  (mapv #(merge % {:rotation {:position (rand-int (:n %))}}) shapes))

(defn reset-rotations [shapes]
  (mapv #(merge % {:rotation {:position 0 :ease 0}}) shapes))

(defn poly-path [[x y r] sides]
  (let [radius (c/radii sides)
        steps (map #(+ % r (/ (c/alphas sides) 2)) (c/steps sides))]
    (vec
      (for [angle steps]
        [(round-to 2 (+ x (* radius (Math/sin angle))))
         (round-to 2 (+ y (* radius (Math/cos angle))))]))))

(defn add-path [shape]
  (assoc shape :path (poly-path (:location shape) (:n shape))))

(defn add-ids [x]
  (into {} (map-indexed
             (fn [i m]
               [i (assoc m :id i)])
             x)))

(defn mk-level
  "Create a level from raw data???"                         ;; TODO: more docstring
  [start-location data [start-index end-index] colours channels]
  (let [shapes0 (mk-shapes start-location data)
        shapes1 (round-shapes shapes0)
        [shapes2 width height] (centre shapes1)
        start (repeat (count channels) start-index)
        end (repeat (count channels) end-index)
        shapes25 (blank-wires shapes2 (count channels))
        shapes3 (add-endpoint-wiring shapes25 start-index (count channels) 0)
        shapes4 (add-endpoint-wiring shapes3 end-index (count channels) 1)
        shapes5 (add-neighbours shapes4)
        shapes6 (reset-rotations shapes5)
        shapes7 (mapv add-path shapes6)
        shapes8 (add-ids shapes7)]
    {:shapes   shapes8
     :width    width
     :height   height
     :channels channels
     :colours  colours
     :start    start
     :end      end}))

(defn wire [level shape-id wiring]
  (assoc-in level [:shapes shape-id :wiring] wiring))

(def blue-on-orange [[250 175 0] [0 0 250] [0 150 225]])
(def orange-yellow-red-channels [[250 175 0] [200 250 0] [250 100 0]])

(def level-one
  (-> (mk-level
        [0 0 c/PI]
        [4 [4 [0 []
               4 [0 []
                  4 [0 []
                     4 [0 []
                        4 [0 []
                           4 []]]]]]]]
        [0 6]
        blue-on-orange
        (butlast (butlast orange-yellow-red-channels)))
      (wire 1 [[[0 2]]])
      (wire 2 [[[0 2]]])
      (wire 3 [[[1 3]]])
      (wire 4 [[[0 2]]])
      (wire 5 [[[0 2]]])))

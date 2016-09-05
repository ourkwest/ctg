(ns hexatile.constants)




(def PI Math/PI)
(def TAU (* 2 PI))
(def TAU_3RD (/ TAU 3))
(def TAU_4TH (/ TAU 4))
(def TAU_6TH (/ TAU 6))
(def TAU_8TH (/ TAU 8))
(def TAU_12TH (/ TAU 12))
(def ROOT_TWO (Math/sqrt 2))
(def ROOT_THREE (Math/sqrt 3))



(def shape-side-length 50)
(defn shape-pad [inner-radius]
  (* inner-radius 1.05))

(def square-side shape-side-length)
(def square-inner-radius (/ square-side 2))
(def square-radius (* square-inner-radius ROOT_TWO))
(def square-pad (shape-pad square-inner-radius))

(def hex-side shape-side-length)
(def hex-radius hex-side)
(def hex-inner-radius (* hex-side (/ ROOT_THREE 2)))
(def hex-pad (shape-pad hex-inner-radius))

(def oct-side shape-side-length)
(def oct-radius (Math.sqrt (+ (* (/ oct-side 2) (/ oct-side 2))
                              (* (+ (/ oct-side 2) (/ oct-side ROOT_TWO)) (+ (/ oct-side 2) (/ oct-side ROOT_TWO))))))
(def oct-inner-radius (* oct-side (/ (+ 1 ROOT_TWO) 2)))
(def oct-pad (shape-pad oct-inner-radius))

(def tri-side shape-side-length)
(def half-tri-side (/ tri-side 2))
(def tri-radius (/ half-tri-side (/ ROOT_THREE 2)))
(def tri-inner-radius (/ tri-radius 2))
(def tri-pad (shape-pad tri-inner-radius))

;;;;;

(def pad-diff (- hex-pad hex-inner-radius))

(def tri-adjust (/ (- tri-pad pad-diff) tri-inner-radius))
(def tri-radius (* tri-radius tri-adjust))
(def tri-inner-radius (* tri-inner-radius tri-adjust))

(def square-adjust (/ (- square-pad pad-diff) square-inner-radius))
(def square-radius (* square-radius square-adjust))
(def square-inner-radius (* square-inner-radius square-adjust))

(def oct-adjust (/ (- oct-pad pad-diff) oct-inner-radius))
(def oct-radius (* oct-radius oct-adjust))
(def oct-inner-radius (* oct-inner-radius oct-adjust))


(def alphas {3 TAU_3RD
             4 TAU_4TH
             6 TAU_6TH
             8 TAU_8TH})

(def radii {3 tri-radius
            4 square-radius
            6 hex-radius
            8 oct-radius})
(def inner-radii {3 tri-inner-radius
                  4 square-inner-radius
                  6 hex-inner-radius
                  8 oct-inner-radius})
(def pads {3 tri-pad
           4 square-pad
           6 hex-pad
           8 oct-pad})

(def steps {3 (range 0 TAU TAU_3RD)
            4 (range 0 TAU TAU_4TH)
            6 (range 0 TAU TAU_6TH)
            8 (range 0 TAU TAU_8TH)})
(ns hexatile.levels
  (:require
    [hexatile.constants :as c]))



(defn polygon [x y sides]
  (let [radius (c/radii sides)
        steps (c/steps sides)]
    (vec
      (for [angle steps]
        [(+ x (* radius (Math/sin angle)))
         (+ y (* radius (Math/cos angle)))]))))

(defn shape [x y rotation sides]
  {:r      {:start-angle   rotation
            :offset-places 0
            :proportion    0}
   :center [x y]
   :points (polygon x y sides)
   :sides  sides
   :wires  (vec (repeat sides nil))})

(defn add-ids [x]
  (into {} (map-indexed
             (fn [i m]
               [(str "s" i) (assoc m :id (str "s" i))])
             x)))

(defn get-first-level []
  {:title   "In The Beginning, There Were No Instructions."
   :palette {:background [255 150 150]
             :foreground [0 50 50]}
   :shapes  (add-ids [(shape 100 100 c/TAU_8TH 4)
                      (shape 100 150 c/TAU_8TH 4)
                      (shape 100 200 c/TAU_8TH 4)
                      ])

   ;:shapes [{:r [:start-angle 0 :offset-places 0 :percentage-ease 0]
   ;          :center [100 100]
   ;          :points [[75 75] [75 125] [125 125] [125 75]]
   ;          :sides 4
   ;          :wires [nil nil 1 nil]}
   ;         {:r [:start-angle 0 :offset-places 0 :percentage-ease 0]
   ;          :center [100 100]
   ;          :points [[75 75] [75 125] [125 125] [125 75]]
   ;          :sides 4
   ;          :wires [0 nil 2 nil]}
   ;         {:r [:start-angle 0 :offset-places 0 :percentage-ease 0]
   ;          :center [100 100]
   ;          :points [[75 75] [75 125] [125 125] [125 75]]
   ;          :sides 4
   ;          :wires [1 nil nil nil]}]

   }
  )


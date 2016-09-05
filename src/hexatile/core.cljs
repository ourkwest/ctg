(ns hexatile.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [hexatile.levels :as levels]
            [hexatile.color :as color]
            [hexatile.constants :as c]))

(enable-console-print!)

(println "Loading core namespace.")

(defonce app-state (atom (levels/get-first-level)))

(defn click [id]
  (swap! app-state update-in [:shapes id :r :offset-places] inc)
  (swap! app-state update-in [:shapes id :r :proportion] #(inc %)))

(defn points-str [points]
  (string/join " " (map #(string/join "," %) points)))

(defn rad->deg [radians]
  (* (/ radians c/TAU) 360))

(defn hello-world []

  (let [state @app-state]

    [:div [:h1 (-> state :title)]

     [:svg {:style    {:width            "50%"
                       :height           "50%"
                       :background-color (color/rgba (-> state :palette :background))}
            :view-box (string/join " " [0 0 500 500])}

      [:g
       (for [[id {:keys [center points sides r]}] (-> state :shapes)]

         [:polygon {:id        id
                    :key id
                    :on-click  #(click id)
                    :points    (points-str points)
                    :style     {:fill   :lime
                                :stroke :purple}
                    :transform (str "rotate(" (rad->deg (+ (:start-angle r)
                                                           (* (:offset-places r) (c/alphas sides))
                                                           (- (* (:proportion r) (c/alphas sides)))))
                                    "," (first center)
                                    "," (second center) ")")}]


         )]

      [:rect {:x         0 :y 0 :width 100 :height 100
              :style     {:fill "rgba(0,250,250,0.75)"}
              :transform "rotate(45)"}]
      ]

     ]))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []

  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn rotate-each [{:keys [start-angle offset-places proportion]}]
  {:start-angle   start-angle
   :offset-places (mod offset-places 24)
   :proportion    (max 0 (- proportion 0.05))})

(defn rotate-all [shapes]
  (into {} (for [[id shape] shapes]
             [id (update shape :r rotate-each)])))

(defn tick [app-state]
  (update app-state :shapes rotate-all))

(defn init []
  (js/setInterval #(swap! app-state tick) 10))

(defonce started (init))

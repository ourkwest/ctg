(ns hexatile.core
  (:require [reagent.core :as reagent :refer [atom]]
            [clojure.string :as string]
            [hexatile.levels :as levels]
            [hexatile.color :as color]
            [hexatile.constants :as c]))

(enable-console-print!)

(println "Loading core namespace.")

(defonce app-state (atom levels/level-one))

(defn click [id]
  (swap! app-state update-in [:shapes id :rotation :position] inc)
  (swap! app-state update-in [:shapes id :rotation :ease] inc))

(defn points-str [points]
  (string/join " " (map #(string/join "," %) points)))

(defn rad->deg [radians]
  (* (/ radians c/TAU) 360))

(defn wire-path [[[x1 y1] [x2 y2] [x3 y3] [x4 y4]]]
  (str "M" x1 " " y1 " C " x2 " " y2 "," x3 " " y3 "," x4 " " y4 ","))

(defn hello-world []
  (let [state @app-state
        channels (:channels state)
        [level-bg shape-stroke shape-fill] (:colours state)]
    [:div {:class "big"
           :style {:background-color (color/rgba level-bg)}}
     [:svg {:style    {:width            "100%"
                       :height           "100%"}
            :view-box (string/join " " [0 0 (:width state) (:height state)])}
      [:g
       (for [[id {:keys [n path location rotation wiring]}] (-> state :shapes)]

         (let [element-id (str "shape-" id)
               [x y r] location
               degrees (rad->deg (+ r
                              (* (:position rotation) (c/alphas n))
                              (- (* (:ease rotation) (c/alphas n)))))
               ;degrees (rad->deg (+ r (* (:position rotation) (c/alphas n))))
               ]

           [:g {:key (str "g-" id)
                :on-click  #(click id)
                :transform (str "rotate(" degrees
                                "," x
                                "," y ")")}

            [:polygon {:id        element-id
                       :key       (str "sf-" id)
                       :points    (points-str path)
                       :style     {:fill   (color/rgba shape-fill)
                                   :stroke :none}}]

            (for [[channel-index channel-wiring] (map-indexed vector wiring)]
              (for [[wire-index [_ _ points]] (map-indexed vector channel-wiring)]
                [:path {:key (str "p-" id "-" channel-index "-" wire-index)
                        :d   (wire-path points)
                        :stroke (color/rgba level-bg)
                        :stroke-width 1
                        :fill :none}
                      ]

                ))

            [:polygon {:id        element-id
                       :key       (str "ss-" id)
                       :points    (points-str path)
                       :style     {:fill   :none
                                   :stroke (color/rgba shape-stroke)}}]
            ]

           ))]]]))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []

  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn rotate-each [{:keys [position ease]}]
  {:position (mod position 24)
   :ease     (max 0 (- ease 0.05))})

(defn rotate-all [shapes]
  (into {} (for [[id shape] shapes]
             [id (update shape :rotation rotate-each)])))

(defn tick [app-state]
  (update app-state :shapes rotate-all))

(defn init []
  (js/setInterval #(swap! app-state tick) 10))

(defonce started (init))

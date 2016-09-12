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

(defn hello-world []
  (let [state @app-state]
    [:div [:h1 (-> state :title)]
     [:svg {:style    {:width            "100%"
                       :height           "50%"
                       :background-color (color/rgba (-> state :colours first))}
            :view-box (string/join " " [0 0 (:width state) (:height state)])}
      [:g
       (for [[id {:keys [n path location rotation]}] (-> state :shapes)]
         (let [element-id (str "shape-" id)
               [x y r] location
               degrees (rad->deg (+ r
                              (* (:position rotation) (c/alphas n))
                              (- (* (:ease rotation) (c/alphas n)))))]
           [:polygon {:id        element-id
                      :key       element-id
                      :on-click  #(click id)
                      :points    (points-str path)
                      :style     {:fill   :lime
                                  :stroke :purple}
                      :transform (str "rotate(" degrees
                                      "," x
                                      "," y ")")}]))]]]))

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

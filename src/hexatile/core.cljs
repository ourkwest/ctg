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
  (swap! app-state update-in [:shapes id :rotation :position] dec)
  (swap! app-state update-in [:shapes id :rotation :ease] dec))

(defn points-str [points]
  (string/join " " (map #(string/join "," %) points)))

(defn rad->deg [radians]
  (* (/ radians c/TAU) 360))

(defn wire-path [[[x1 y1] [x2 y2] [x3 y3] [x4 y4]]]
  (str "M" x1 " " y1 " C " x2 " " y2 "," x3 " " y3 "," x4 " " y4 ","))

(def on-width 3)
(def off-width 1)
(def blob-size 2)
(def blob-spacing 22)


(defn render-wiring [id wiring flow dash-offset channels]
  [:g {:key (str "pg-" id)}
   (let [paths
         (for [[channel-index channel-wiring] (map-indexed vector wiring)
               [wire-index [_ _ points]] (map-indexed vector channel-wiring)]
           (let [forwards (get flow [channel-index id wire-index 1])
                 backwards (get flow [channel-index id wire-index 0])
                 either (or forwards backwards)]
             [(if (not either)
                [:path {:key          (str "pl-" id "-" channel-index "-" wire-index)
                        :d            (wire-path points)
                        :stroke       (color/rgba [0 0 0 0.5])
                        :stroke-width (inc off-width)
                        :fill         :none}]
                [:path {:key          (str "pl-" id "-" channel-index "-" wire-index)
                        :d            (wire-path points)
                        :stroke       (color/rgba [255 255 255 0.5])
                        :stroke-width (inc on-width)
                        :fill         :none}])
              (when (not either)
                [:path {:key          (str "p-" id "-" channel-index "-" wire-index)
                        :d            (wire-path points)
                        :stroke       (color/rgba (channels channel-index))
                        :stroke-width off-width
                        :fill         :none}])
              (when either
                [:path {:key          (str "p-" id "-" channel-index "-" wire-index)
                        :d            (wire-path points)
                        :stroke       (color/rgba (channels channel-index))
                        :stroke-width on-width
                        :fill         :none}])
              (when forwards
                [:path {:key              (str "pf-" id "-" channel-index "-" wire-index)
                        :d                (wire-path points)
                        :style            {:stroke-dashoffset (- dash-offset)}
                        :stroke           :black
                        :stroke-dasharray (str blob-size ", " blob-spacing)
                        :stroke-width     blob-size
                        :fill             :none}])
              (when backwards
                [:path {:key              (str "pb-" id "-" channel-index "-" wire-index)
                        :d                (wire-path points)
                        :style            {:stroke-dashoffset dash-offset}
                        :stroke           :black
                        :stroke-dasharray (str blob-size ", " blob-spacing)
                        :stroke-width     blob-size
                        :fill             :none}])]))]
     (remove nil? (apply concat (apply mapv vector paths))))])


(defn hello-world []
  (let [state @app-state
        channels (:channels state)
        flow (:flow state)
        now (:time state)
        dash-offset (mod (/ now 50) (+ blob-size blob-spacing))
        [level-bg shape-stroke shape-fill] (:colours state)]

    ;(println now)

    [:div {:class "big"
           :style {:background-color (color/rgba level-bg)}}
     [:svg {:style    {:width            "100%"
                       :height           "100%"}
            :view-box (string/join " " [0 0 (:width state) (:height state)])}
      [:g
       (for [[id {:keys [n path location rotation wiring]}] (-> state :shapes)]

         (let [element-id (str "shape-" id)
               [x y r] location
               degrees (- (rad->deg (+ r
                                       (* (:position rotation) (c/alphas n))
                                       (- (* (:ease rotation) (c/alphas n))))))
               ;degrees (rad->deg (+ r (* (:position rotation) (c/alphas n))))
               ]

           [:g {:key (str "g-" id)
                :on-click  #(click id)
                :transform (str "rotate(" degrees
                                "," x
                                "," y ")")}

            [:g {:class "hoverable"}
             [:polygon {:id     element-id
                        :key    (str "sf-" id)
                        :points (points-str path)
                        :style  {:fill   (color/rgba shape-fill)
                                 :stroke :none}}]

             (render-wiring id wiring flow dash-offset channels)]

            [:polygon {:id        element-id
                       :key       (str "ss-" id)
                       :points    (points-str path)
                       :style     {:fill   :none
                                   :stroke (color/rgba shape-stroke)}}]

            (comment "debug"
                     [:line {:x1    (first (first path))
                             :y1    (second (first path))
                             :x2    (inc (first (first path)))
                             :y2    (inc (second (first path)))
                             :style {:stroke       :red
                                     :stroke-width 3}}]

                     [:line {:x1    x
                             :y1    y
                             :x2    x
                             :y2    (- y 20)
                             :style {:stroke :black}}])
            ]

           ))]]]))

(reagent/render-component [hello-world]
                          (. js/document (getElementById "app")))

(defn on-js-reload []

  (reset! app-state levels/level-one)
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn rotate-each [{:keys [position ease]}]
  {:position (mod position 24)
   :ease     (min 0 (+ ease 0.05))})

(defn rotate-all [shapes]
  (into {} (for [[id shape] shapes]
             [id (update shape :rotation rotate-each)])))

(defn index-of
  "return the index of the supplied item, or nil"
  [coll item]
  (let [len (count coll)]
    (loop [i 0]
      (cond
        (<= len i)         nil,
        (= item (get coll i)) i,
        :else              (recur (inc i ))))))

(defn do-flow [done cursors shapes]
  ; pick a cursor
  ; if it is already in done:
  ;   start again
  ; else:
  ;   generate next generation of cursors
  ;   remove this cursor from cursors
  ;   add this cursor to done
  ;   start again
  (let [[channel-index shape-index wire-index direction-index :as this-one] (first cursors)
        fewer-cursors (disj cursors this-one)]
    (cond
      (nil? this-one) done
      (done this-one) (do-flow done fewer-cursors shapes)
      (not= 0 (get-in shapes [shape-index :rotation :ease] 0)) (do-flow done fewer-cursors shapes)
      :else
      (let [more-done (conj done this-one)
            exit-side (get-in shapes [shape-index :wiring channel-index wire-index direction-index])
            rotated-exit-side (mod (+ exit-side (get-in shapes [shape-index :rotation :position]))
                                   (get-in shapes [shape-index :n]))
            neighbour-index (get-in shapes [shape-index :neighbours rotated-exit-side])]
        (if-not neighbour-index
          (do-flow more-done fewer-cursors shapes)
          (if-let [;; find this shape's index in the (rotated) neighbour's neighbours
                   enter-side (index-of (get-in shapes [neighbour-index :neighbours]) shape-index)]
            ;; un-rotate the index
            (let [rotated-enter-side (mod (- enter-side (get-in shapes [neighbour-index :rotation :position]))
                                          (get-in shapes [neighbour-index :n]))
                  channel-wiring (get-in shapes [neighbour-index :wiring channel-index])
                  ;; find all neighbour's wires that run from that index
                  more-cursors (into fewer-cursors (remove nil?
                                                           (for [wire-index (range (count channel-wiring))]
                                                             (cond
                                                               (= rotated-enter-side (get-in channel-wiring [wire-index 0]))
                                                               [channel-index neighbour-index wire-index 1]
                                                               (= rotated-enter-side (get-in channel-wiring [wire-index 1]))
                                                               [channel-index neighbour-index wire-index 0]))))]
              (do-flow more-done more-cursors shapes))
            (do-flow more-done fewer-cursors shapes)))))))

(defn reflow [{:keys [start end shapes] :as state}]

  ; cursor: [channel-index shape-index wire-index direction-index]

  (let [cursors (set (for [channel-index (range (count (:channels state)))
                           wire-index (range (count (get-in state [:shapes (get start channel-index) :wiring channel-index])))]
                       [channel-index (get start channel-index) wire-index 1]))
        done #{}]
    (-> state
        (assoc :debug [cursors])
        (assoc :flow (do-flow done cursors shapes)))))

(defn tick [app-state]
  ;(println "tick")
  (if (:running app-state)
    (-> app-state
        (assoc :time (.getTime (js/Date.)))
        (update :shapes rotate-all)
        reflow
        )
    app-state))

(defn init []
  (js/setInterval #(swap! app-state tick) 10))

(defonce started (init))

(ns hexatile.color)



(defn rgba [[r g b a]]
  (if a
    (str "rgba(" r \, g \, b \, a \))
    (str "rgb(" r \, g \, b \))))


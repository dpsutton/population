(ns population.viruses)

(defn chance? [threshold]
  (<= (rand) threshold))

(defmulti dies? :virus)

(defmethod dies? :simple [{:keys [clear]}]
  (chance? clear))

(defmulti reproduces? (fn [virus pop-density] (:virus virus)))

(defmethod reproduces? :simple [{:keys [birth]} pop-density]
  (chance? (* birth (- 1.0 pop-density))))

(defmulti reproduce :virus)
(defmethod reproduce :simple [virus]
  virus)

(defn simple-viruses [number max-birth-prob clear-prob]
  (take number
        (repeat {:birth max-birth-prob
                 :clear clear-prob
                 :virus :simple})))

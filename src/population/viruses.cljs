(ns population.viruses)

(defn chance? [threshold]
  (<= (rand) threshold))

(defmulti dies? :virus)

(defmethod dies? :simple [{:keys [clear]} _active-drugs]
  (chance? clear))

(defmethod dies? :resistant [{:keys [clear resistances]} active-drugs]
  (and (every? resistances active-drugs)
       (chance? clear)))

(defmulti reproduces? (fn [virus pop-density active-drugs] (:virus virus)))

(defmethod reproduces? :simple [{:keys [birth]} pop-density _active-drugs]
  (chance? (* birth (- 1.0 pop-density))))

(defmethod reproduces? :resistant [{:keys [birth]} pop-density]
  (chance? (* birth (- 1.0 pop-density))))

(defmulti reproduce :virus)

(defmethod reproduce :simple [virus]
  virus)

(defn mutate-resistances [resistances mutation-probability]
  (into {}
        (map (fn [[drug resistant?]]
               [drug (if (chance? mutation-probability)
                       (not resistant?)
                       resistant?)]))
        resistances))

(defmethod reproduce :resistant
  [{mutation-probability :mutation :as resistant-virus}]
  (update resistant-virus
          :resistances mutate-resistances mutation-probability))

(defmulti initial :virus-type)

(defmethod initial :simple
  [{:keys [number-of-viruses max-birth-prob clear-prob]}]
  (take number-of-viruses
        (repeat {:birth max-birth-prob
                 :clear clear-prob
                 :virus :simple})))

(defmethod initial :resistant
  [{:keys [max-birth-prob clear-prob resistances mutation-prob number-of-viruses]}]
    (take number-of-viruses
          (repeat {:birth max-birth-prob
                   :clear clear-prob
                   :resistances resistances
                   :mutation mutation-prob
                   :virus :resistant})))

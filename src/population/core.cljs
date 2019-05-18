(ns population.core
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            #_["chart.js" :default Chart]
            ["chart.js" :as Chart]))

;; define your app data so that it doesn't get over-written on reload

(def default-db
  {:simulation-results (list)
   :parameters {:number-of-viruses 100
                :max-population 1000
                :max-birth-prob 0.1
                :clear-prob 0.05
                :number-of-trials 300}})

(rf/reg-event-db
  :initialize-db
  (fn [_ _] default-db))

(rf/reg-sub
  :simulation-results
  (fn [db _]
    (:simulation-results db)))

(rf/reg-event-db
  :add-results
  (fn [db [_ {:keys [virus-type parameters results id]}]]
    (update-in db
               [:simulation-results]
               #(take 10 (conj % {:virus-type virus-type
                                  :parameters parameters
                                  :simulation-results results
                                  :id id})))))

(rf/reg-sub
  :get-in
  (fn [db [_ path]]
    (get-in db path)))

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

(defn simulate-simple [{:keys [number-of-viruses max-population
                               max-birth-prob clear-prob number-of-trials]}]
  (let [step (fn step [viruses]
               (loop [alive []
                      q viruses]
                 (if-let [v (first q)]
                   (recur (cond-> alive
                            (not (dies? v)) (conj v)

                            (reproduces? v (/ (+ (count alive)
                                                 (count viruses))
                                              max-population))
                            (conj (reproduce v)))
                          (rest q))
                   alive)))
        initial-viruses (take number-of-viruses
                              (repeat {:birth max-birth-prob
                                       :clear clear-prob
                                       :virus :simple}))
        perform-trial (fn [viruses]
                        (->> initial-viruses
                             (iterate step)
                             (take 300)
                             (map count)))]
    (->> (repeat initial-viruses)
         (take number-of-trials)
         (map perform-trial)
         (apply map +)
         (map #(/ % number-of-trials)))))

(comment
  (time (simulate-simple (-> default-db
                             :parameters
                             (assoc :number-of-trials 100))))
  )

(rf/reg-event-fx
  :simulate-simple
  (fn [_ [_ parameters]]
    {:dispatch [:add-results {:virus-type :simple
                              :parameters parameters
                              :results (simulate-simple parameters)
                              :id (gensym "simple-chart")}]}))

(defn config [{:keys [simulation-results virus-type] :as results}]
  (clj->js
    {:type "line"
     :data {:labels (map #(str "Time " %) (range (count simulation-results)))
            :datasets
            [{:backgroundColor "red"
              :borderColor "red"
              :fill false
              :label (str "Virus count for " (name virus-type) " virus")
              :showLine true
              :data simulation-results}]}
     :options
     {:responsive true
      :scales {:xAxes [{:display true}]
               :yAxes [{:ticks {:beginAtZero true}}]}}}))

(defn chart [results]
  (let [!ref (atom nil)]
    (r/create-class
      {:display-name "results-chart"
       :component-did-mount (fn []
                              (when-let [com @!ref]
                                (Chart. (.getContext com "2d") (config results))))
       :reagent-render
       (fn [{:keys [id] :as results}]
         [:canvas {:ref (fn [com] (reset! !ref com))
                   :id id
                   :width "400px"
                   :height "300px"}])})))

(defn charts [results]
  [:<>
   (doall (for [result results]
            ^{:key (:id result)}
            [:div {:style {:display "flex"
                           :flex-direction "row"
                           :justify-content "center"
                           :align-items "center"
                           :margin "45px"}}
             [:div {:style {:width "800px"
                            :height "600px"}}
              [chart result]]]))])

(defn page []
  (let [parameters @(rf/subscribe [:get-in [:parameters]])
        results @(rf/subscribe [:get-in [:simulation-results]])]
    [:div
     [:button {:on-click #(rf/dispatch [:simulate-simple parameters])} "Simulate"]
     [charts results]]))

(defn start []
  (r/render [page]
            (. js/document (getElementById "app"))))

(defn ^:export init []
  ;; init is called ONCE when the page loads
  ;; this is called in the index.html and must be exported
  ;; so it is available even in :advanced release builds
  (rf/dispatch-sync [:initialize-db])
  (start))

(defn stop []
  ;; stop is called before any code is reloaded
  ;; this is controlled by :before-load in the config
  (js/console.log "stop"))

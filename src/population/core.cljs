(ns population.core
  (:require [reagent.core :as r]
            [re-frame.core :as rf]
            ["chart.js" :as Chart]
            [population.viruses :as virus]))

;; define your app data so that it doesn't get over-written on reload

(def default-db
  {:simulations {:simple {:parameters {:number-of-viruses 100
                                       :max-population 1000
                                       :max-birth-prob 0.1
                                       :clear-prob 0.05
                                       :number-of-trials 100
                                       :virus-type :simple
                                       :active-drugs #{}}
                          :partial-results []
                          :results []}
                 :resistant {:parameters {:number-of-viruses 100
                                          :max-population 1000
                                          :max-birth-prob 0.1
                                          :clear-prob 0.05
                                          :number-of-trials 100
                                          :virus-type :resistant
                                          :resistances {:drug-a false}
                                          :take-drugs {150 #{:drug-a}}
                                          :mutation-prob 0.005
                                          :active-drugs #{}}
                             :partial-results []
                             :results []}}})

(rf/reg-event-db
  :initialize-db
  (fn [_ _] default-db))

(rf/reg-sub
  :simulation-results
  (fn [db [_ sim-type partial|full]]
    (get-in db [:simulations sim-type partial|full])))

(rf/reg-event-db
  :add-results
  (fn [db [_ virus-type results]]
    (-> db
        (update-in [:simulations virus-type :partial-results]
                   into results))))

(rf/reg-sub
  :get-in
  (fn [db [_ path]]
    (get-in db path)))

(rf/reg-event-db
  :assoc-in
  (fn [db [_ path value]]
    (assoc-in db path value)))

(defn simulate [{:keys [number-of-viruses max-population max-birth-prob
                        clear-prob number-of-trials active-drugs take-drugs]
                 :as parameters}]
  (let [step (fn step [{:keys [viruses active-drugs n]}]
               (let [active-drugs (if-let [new-drugs (get take-drugs n)]
                                    (into active-drugs new-drugs)
                                    active-drugs)]
                 (loop [alive []
                        q viruses]
                   (if-let [v (first q)]
                     (let [dies? (virus/dies? v active-drugs)]
                       (recur (cond-> alive
                                (not dies?) (conj v)

                                (and (not dies?)
                                     (virus/reproduces? v (/ (+ (count alive)
                                                                (count viruses))
                                                             max-population)))
                                (conj (virus/reproduce v)))
                              (rest q)))
                     {:viruses alive
                      :n (inc n)
                      :active-drugs active-drugs}))))
        initial-viruses (virus/initial parameters)
        perform-trial (fn [viruses]
                        (->> {:viruses initial-viruses
                              :n 0
                              :active-drugs active-drugs}
                             (iterate step)
                             (take 300)
                             (mapv (comp count :viruses))))]
    (into []
          (comp
            (take number-of-trials)
            (map perform-trial))
          (repeat initial-viruses))))

(comment
  (simulate (-> default-db
                :simulations
                :simple
                :parameters
                (assoc :number-of-trials 1)))

  (simulate (-> default-db
                :simulations
                :resistant
                :parameters
                (assoc :number-of-trials 1)))
  )

(defn partial-to-final
  [{:keys [partial-results parameters] :as simulation}]
  (let [finalized-results (->> partial-results
                               (apply map +)
                               (mapv #(/ % (:number-of-trials parameters))))]
    (-> simulation
        (assoc :partial-results [])
        (assoc :results finalized-results))))

(rf/reg-event-db
  :finalize
  (fn [db [_ virus-type]]
    (let [number-of-trials (get-in db [:simulations
                                       virus-type
                                       :parameters
                                       :number-of-trials])]
      (-> db
          (assoc-in [:simulations virus-type :computing?] false)
          (update-in [:simulations virus-type] partial-to-final)))))

(rf/reg-event-fx
  :compute
  (fn [_ [_ parameters]]
    (let [trials (:number-of-trials parameters)
          chunk-size (if (> trials 20)
                       20
                       trials)
          remaining (- trials chunk-size)
          followup (if (pos? remaining)
                     [:compute (assoc parameters
                                      :number-of-trials
                                      remaining)]
                     [:finalize (:virus-type parameters)])]
      {:dispatch-n [[:add-results (:virus-type parameters)
                     (simulate (assoc parameters :number-of-trials chunk-size))]
                    followup]})))

(rf/reg-event-fx
  :simulate
  (fn [_ [_ {:keys [virus-type] :as parameters}]]
    {:dispatch-n [[:assoc-in [:simulation virus-type :parameters] parameters]
                  [:compute parameters]]}))

(defn config [virus-type results]
  (clj->js
    {:type "line"
     :data {:labels (map #(str "Time " %) (range (count results)))
            :datasets
            [{:backgroundColor "red"
              :borderColor "red"
              :fill false
              :label (str "Virus count for " (name virus-type) " virus")
              :showLine true
              :data results}]}
     :options
     {:responsive true
      :scales {:xAxes [{:display true}]
               :yAxes [{:ticks {:beginAtZero true}}]}}}))

(defn chart-display [sim-type results]
  (let [!ref (atom nil)]
    (r/create-class
      {:display-name "results-chart"
       :component-did-mount (fn []
                              (when-let [com @!ref]
                                (Chart. (.getContext com "2d") (config sim-type results))))
       :reagent-render
       (fn [results]
         [:div {:width "1000px"
                :height "500px"}
          [:canvas {:ref (fn [com] (reset! !ref com))
                    :id (name sim-type)
                    :height "500px"
                    :width "900px"}]])})))

(defn chart-description [parameters]
  [:div
   [:ul.list-group
    (doall (for [k (-> parameters keys sort)]
             ^{:key (name k)}
             [:li.list-group-item (name k) ": " (k parameters)]))]])

(defn chart-container [parameters results]
  [:div {:style {:display "flex"
                 :flex-direction "row"
                 :flex-wrap "wrap"
                 :justify-content "center"
                 :align-items "center"
                 :margin "85px"}}
   [chart-display (:virus-type parameters) results]
   [chart-description parameters]])

(defn display-results [{:keys [parameters results partial-results computing?] :as simulation}]
  [:div
   (when computing?
     [:h1 "Performed "
      (-> partial-results count)
      " of "
      (-> parameters
          :number-of-trials)
      " trials."])
   (when (seq results)
     [chart-container parameters results])])

(defn page []
  (let [simple @(rf/subscribe [:get-in [:simulations :simple]])
        resistant @(rf/subscribe [:get-in [:simulations :resistant]])
        computing? (some :computing? [simple resistant])
        simulate! #(do (rf/dispatch-sync [:assoc-in [:simulations (get-in % [:parameters :virus-type]) :computing?] true])
                       (rf/dispatch [:simulate (:parameters %)]))]
    [:div.container
     [:div#buttons {:style {:margin-top "4vh"
                            :margin-bottom "3vh"
                            :display "flex"
                            :flex-direction "row"
                            :justify-content "space-between"}}
      [:button {:type :button
                :class "btn btn-dark"
                :on-click #(simulate! simple)
                :disabled computing?} "Simulate Simple Virus"]
      [:button {:type :button
                :class "btn btn-dark"
                :on-click #(simulate! resistant)
                :disabled computing?} "Simulate Drug-Resistant Virus"]]
     [:div#charts
      [display-results simple]
      [display-results resistant]]]))

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

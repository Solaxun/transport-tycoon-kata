(ns transport-tycoon-kata.core
  (:gen-class))

(defn init-state [workers deliveries]
  {:time 0
   :workers workers
   :work-in-progress []
   :busy []
   :deliveries deliveries})

(defn time-left [time-type]
  (get {:working {"A" 5 "B" 5}
        :busy    {"A" 2 "B" 10}} time-type))

(defn perform-work [time-func new-workers coll]
  (->> (into coll (map time-func new-workers))
       (map dec)
       (filter pos?)))

(defn travel
  [{:keys [time workers work-in-progress busy deliveries] :as state}]
  (let [available (- workers (count busy))
        newly-working (take available deliveries)]
    (-> state
        (update :time inc)
        (update :work-in-progress (partial perform-work (time-left :working) newly-working))
        (update :busy (partial perform-work (time-left :busy) newly-working))
        (update :deliveries (partial drop available)))))

(defn travel-time [& {:keys [workers deliveries]}]
  (:time (some (fn [state]
                 (when (every? empty? [(:work-in-progress state)
                                       (:deliveries state)])
                   state))
               (iterate travel (init-state workers deliveries)))))

(travel-time :workers 2 :deliveries ["A" "A" "B" "A" "B" "B" "A" "B"])

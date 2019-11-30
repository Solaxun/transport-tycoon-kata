(ns transport-tycoon-kata.transport
  (:gen-class))

(defn warehouse [workers deliveries]
  {:workers workers
   :travel-time {"A" 1 "B" 5}
   :deliveries deliveries
   :busy []
   :working []})

(defn shipyard [workers deliveries]
  {:workers workers
   :travel-time {"A" 4}
   :deliveries deliveries
   :busy []
   :working []})

(defn init-worker [dest]
  (get {"A" {:dest "A" :time nil :stops [:shipyard]}
        "B" {:dest "B" :time nil :stops []}}
       dest))

(defn init-state [workers deliveries]
  {:time 0
   :waypoints {:warehouse (warehouse workers (map init-worker deliveries))
               :shipyard (shipyard 1 [])}})

(defn perform-work [time-func new-workers workers]
  (->> (into workers (map time-func new-workers))
       (map #(update % :time dec))))

(defn time-left [time-func multiplier]
  (fn [worker]
    (assoc worker :time (-> worker :dest time-func (* multiplier)))))

(defn travel [[wp-name wp]]
  (let [{:keys [workers travel-time deliveries busy working] :as wp} wp
        available (- workers (count busy))
        new-workers (take available deliveries)]
    (-> wp
        (update :working (partial perform-work (time-left travel-time 1) new-workers))
        (update :busy (partial perform-work (time-left travel-time 2) new-workers))
        (update :busy (partial filter (fn [worker] (-> worker :time pos?))))
        (update :deliveries (partial drop available))
        (#(hash-map wp-name %)))))

(defn transfers [state]
  (reduce (fn [state worker]
            (if (and (zero? (:time worker))
                     (not-empty (:stops worker)))
              (update-in state
                         [:waypoints (-> worker :stops first) :deliveries]
                         conj
                         (update worker :stops next))
              state))
          state
          (mapcat :working (vals (:waypoints state)))))

(defn remove-finished-workers [[wp-name wp]]
  {wp-name (update wp :working  #(filter  (fn [w] (-> w :time pos?)) %))})

(defn move-deliveries
  [{:keys [time waypoints] :as state}]
  (-> state
      (update :time inc)
      (update :waypoints (comp (partial apply merge) (partial map travel)))
      (transfers)
      (update :waypoints (comp (partial apply merge) (partial map remove-finished-workers)))))

(defn total-travel-time [& {:keys [workers deliveries]}]
  (:time
   (some (fn [state]
           (when (every? empty?
                         (mapcat #(vals (select-keys % [:deliveries :working]))
                                 (vals (:waypoints state))))
             state))
         (iterate move-deliveries (init-state workers deliveries)))))

(total-travel-time :workers 2 :deliveries ["A" "A" "B" "A" "B" "B" "A" "B"])

(ns unfinished-story.combat
  (:require [unfinished-story.creature :as creature])
  (:require [clojure.data.priority-map :as priority]))

(declare map-win-lose bind hit-event own-group
         hit dam-fun-supplier find-first
         find-by-id target-group try-find-by find-by with-priority indices)


;; Auto battles with only attack actions.
(defn auto-combat [state]
    (let [actor-id (first (first (state :priority-map))) priority-queue (state :priority-map)]
      (if (and (some creature/alive? (state :attackers))
               (some creature/alive? (state :defenders)))

          (let [actor (lookup-actor state actor-id)
                enemy-group (target-group state actor)
                target (first (state enemy-group))]

            (with-priority state (dissoc priority-queue actor-id))

              (assoc-in state [enemy-group (indices (fn [x] (= (x :id) (target :id)))
                                        (state (own-group target state)))]
                        (first (hit actor target))))

      )))

;; Retrieve information about creature: creature, group and index
(defn lookup-actor [state actor-id]
    (find-by-id (concat (state :attackers) (state :defenders)) actor-id)
  )

;;Updates state with new prioriy-map
(defn with-priority [state new-priorities]
  (assoc state :priority-map new-priorities))

;;Updates state with creature updates.
(defn with-creature [state creature]
  (assoc state :priority-map creature))

;; Return group opposite to grop in each current creature is.
(defn target-group [state creature]
  (if (try-find-by (state :attackers) :id (creature :id))
    :defenders :attackers))

;; Return group opposite to actors in each current creature is.
(defn own-group [state creature]
  (if (try-find-by (state :attackers) :id (creature :id))
    :attackers :defenders))


;; Find element index in vector
(defn indices [pred coll]
   (first (keep-indexed #(when (pred %2) %1) coll)))

;; Finds random target between creatures still alive in enemy group.
(defn find-target [coll] (find-first creature/alive? coll))


;; Finds create by id in combat state. Result can be nil.
(defn try-find-by [coll field val]
  (find-first (fn [x] (= (x field) val)) coll)
  )

;; Finds create by id in combat state. Result should be present.
(defn find-by-id [coll id]
  (find-by coll :id id)
  )

;; Finds create by id in combat state. Result should be present.
(defn find-by [coll field val]
  {:post [(some? %)]}
  (find-first (fn [x] (= (x field) val)) coll)
  )

;;Find first element in collection or throw AssertionError.
(defn find-first [f coll]
  (first (filter f coll))
  )

;;Performs the battle of two groups, in a automatic manner
(defn combat-step [combat-state]
  )

;;Creates initial battle state. Retuns map of creatures with binding to battle side and populate the
;;priority map.
(defn init-battle
  "Creates initial battle state."
  [attackers defenders]
  {:pre [(not-empty attackers) (not-empty defenders)]}
  (let [p (priority/priority-map)]
    {:attackers attackers
     :defenders defenders
     :system-time 0
     :priority-map (into p (into [] (map (fn [c] [(c :id) (creature/initiative c)]) (concat attackers defenders))))}
  ))


;;Creates a creature group
(defn create-group [creatures]
  {:creatures creatures})

;;Performs a battle between two creatures till one of them wins.
;;Return both creatures and a even battle log.
(defn two-creature-battle [attacker defender queue]
  (if (every? creature/alive? [attacker defender])
    (let [hit-result (hit attacker defender)]
      (two-creature-battle (first hit-result) attacker (conj queue (last hit-result))))
    (map-win-lose attacker defender queue)))

;;One creature hits another one for certain amount of damage.
(defn hit
  ([attacker defender] (hit attacker defender (dam-fun-supplier)))
  ([attacker, defender, damage-fun]
  (let [damage (damage-fun (attacker :damage))]
    [(assoc defender :cur-hp
      (- (get defender :cur-hp) damage)) (hit-event attacker defender damage)])))

;; Collect winner/looser map
(defn map-win-lose [first, second, queue]
  (if (creature/alive? first)
    {:winner first :looser second :queue queue}
    {:winner second :looser first :queue queue}))


;; Event collector monada
(defn event-bind [[x queue] f]
  (let [[nx nqueue] (f x)]
    [nx (vec (concat queue nqueue))]))

(defn hit-event [who whom damage]
  (let [get-name (fn [x] (get x :name))]
  {:type :hit-event :who (get-name who) :whom (get-name whom) :damage damage}))


;;Creates randomize damage object.
(defn rand-damage[damage]
  (+ 1 (rand-int damage)))

(defn dam-fun-supplier [] rand-damage)

(ns unfinished-story.combat
  (:require [unfinished-story.creature :as creature])
  (:require [clojure.data.priority-map :as priority]))

(declare map-win-lose bind hit-event hit dam-fun-supplier find-first)


;; Auto battles with only attack actions.
(defn auto-combat [init-state]
    (let [actor (first (init-state :priority-map))])
  )

;; Finds create by id in combat state.
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
     :priority-map (conj p (map (fn [c] [(c :id) (creature/initiative c)]) (concat attackers defenders)))}
  ))


;;Creates a creature group
(defn create-group [creatures]
  {:creatures creatures})

;;Performs a battle between two creatures till one of them wins.
;;Return both creatures and a even battle log.
(defn two-creature-battle [attacker defender queue]
  (if (every? creature/alive? [attacker defender])
    (let [hit-result (hit attacker defender (dam-fun-supplier))]
      (two-creature-battle (first hit-result) attacker (conj queue (last hit-result))))
    (map-win-lose attacker defender queue)))

;;One creature hits another one for certain amount of damage.
(defn hit[attacker, defender, damage-fun]
  (let [damage (damage-fun (get attacker :damage))]
    [(assoc defender :cur-hp
      (- (get defender :cur-hp) damage)) (hit-event attacker defender damage)]))

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

(ns unfinished-story.creature)

(declare alive?)

(def max-initiative 20)

(def default-action-cost 10)

(defn create-creature [params]
  {:post (not-empty (params :id))}
  params)

(defn create-test-creature []
  (create-creature {:id (gensym "creature-")
                    :name "Test Creature"
                    :max-hp 100
                    :cur-hp 100,
                    :damage 10}))


;;Checks if creature is alive
(defn alive? [creature] (< 0 (get creature :cur-hp)))

;; Give starting postion for creature, the lesser the faster creature will act.
(defn initiative
  "Calculates creature intiative in battle."
  [creature] (inc (rand-int max-initiative)))

;; Defines action time for creature
(defn action-cost[creature action] default-action-cost)

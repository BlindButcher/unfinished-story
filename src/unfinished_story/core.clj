(ns unfinished-story.core
  (:require [unfinished-story.combat :as combat])
  (:require [unfinished-story.creature :as creature]))




(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(creature/create-test-creature)




(combat/two-creature-battle (creature/create-test-creature) (creature/create-test-creature) [])




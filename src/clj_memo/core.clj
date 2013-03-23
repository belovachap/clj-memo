(ns clj-memo.core)

(defn create-card
  "Create a new card."
  [question answer]
  {:question question 
   :answer answer
   :easy-factor 2.5
   :schedule []}
)

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

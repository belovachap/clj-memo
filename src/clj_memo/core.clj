(ns clj-memo.core
  (:require [clj-time.core :as clj-time]
            [clj-time.local :refer :all]))

(comment
  (def ^:dynamic *card-deck* []))

(comment
  (defn create-card
    "Create a new card."
    [question answer]
    (atom {:question question 
           :answer answer
           :easy-factor 2.5
           :last-review-date nil
           :next-review-date nil
           :high-quality-reviews 0})))

(comment
  (defn next-easy-factor [last-easy-factor recall]
    (+ last-easy-factor (- 0.1 (* (- 5 recall) (+ 0.08 (* (- 5 recall) 0.02)))))))

(comment
  (defn next-review-interval
    "Determine how many days to review the card within."
    [card]
    (let [{:keys [easy-factor schedule]} @card]
      (cond 
       (= (count schedule) 0) 1
       (= (count schedule) 1) 6
       :else (int (* (:review-within-days (last schedule)) easy-factor))))))

(comment
  (defn update-card [card recall]
    (let [card @card]
      (update-in card [:easy-factor] next-easy-factor recall)
      (update-in card [:schedule] conj {:from (local-now) 
                                        :review-within-days (next-review-interval)}))))

(comment
  (defn update-card! [card recall]
    (swap! card update-card recall)))

(comment
  (defn main
    "Run the training program"
    []
    ;; Get the cards that need to be reviewed today
    ;; Show cards and calculate next review date
    (doseq [card (cards-to-review (local-now))]
      (review-card card))
    ))

(comment
  (review-card
   [card]
   (let [recall (prompt-card card)]
     (update-card card recall))))

(defn review-card? [date card]
  "Determine if the card should be reviewed with respect to the date."
  (let [{:keys [next-review-date]} @card]
    (if (nil? next-review-date) true
        ((comp not clj-time/after?) next-review-date date))))

(defn cards-to-review
  "Return the collection of cards that need to be reviewed today"
  [date deck]
  (filter (partial review-card? date) deck))

(defn prompt-card
  "Display card at prompt and return recall."
  [card]
  (let [{:keys [question answer]} @card]
    (println question)
    (println "Hit enter for answer...")
    (read-line)
    (println answer)
    (println "How would you rate your recall:")
    (read)))
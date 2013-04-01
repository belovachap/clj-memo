;; Clojure implementation of SuperMemo http://www.supermemo.com/english/ol/sm2.htm
(ns clj-memo.core
  (:require 
   [clojure.contrib.math :refer [ceil]]
   [clj-time.core :as clj-time]
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
           :last-recall nil
           :high-quality-reviews 0})))

(defn update-easy-factor [easy-factor recall]
  (if (< recall 3) easy-factor
      (+ easy-factor (- 0.1 (* (- 5 recall) (+ 0.08 (* (- 5 recall) 0.02)))))))

(defn update-high-quality-reviews
  [high-quality-reviews recall]
  (if (< recall 3) 0 (inc high-quality-reviews)))

(defn update-review-interval
  "Determine how many days to review the card within."
  [review-interval high-quality-reviews easy-factor]
  (case high-quality-reviews
    0 1
    1 6
    (int (ceil (* review-interval easy-factor)))))

(comment
  (defn update-card [card recall review-date]
    (let [{:keys easy-factor last-review-date next-review-date last-recall high-quality-reviews} card
          new-last-recall recall
          new-last-review-date review-date
          new-high-quality-reviews (update-high-quality-reviews high-quality-reviews recall)
          new-easy-factor (update-easy-factor easy-factor recall)
          new-next-review-date (update-next-review-date new-high-quality-reviews new-easy-factor last-review-date next-review-date)]
      (assoc card 
        :easy-factor new-easy-factor
        :last-review-date new-last-review-date
        :next-review-date new-next-review-date
        :last-recall new-last-recall
        :high-quality-reviews new-high-quality-reviews))))

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
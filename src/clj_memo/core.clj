(ns clj-memo.core
  (:require [clj-time.core :as clj-time]))

(def card-deck [])

(defn create-card
  "Create a new card."
  [question answer]
  {:question question 
   :answer answer
   :easy-factor 2.5
   :schedule []})

(defn review-card-by-date
  "Determine the date by which the card should be reviewed."
  [card]
  (clj-time/plus (:from (last (:schedule card))) (clj-time/days (:review-within-days (last (:schedule card))))))

(defn review-card-within-days
  "Determine how many days to review the card within."
  [card]
  (cond 
   (= (count (:schedule card)) 0) 1
   (= (count (:schedule card)) 1) 6
   :else (int (* (:review-within-days (last (:schedule card))) (:easy-factor card)))))

(defn next-card-to-review
  "Find the card with the closest due date."
  [deck]
  (first (sort-by review-card-by-date deck)))
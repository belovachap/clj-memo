(ns clj-memo.core
  (:require [clj-time.core :as clj-time]
            [clj-time.local :refer :all]))

(def ^:dynamic *card-deck* [])

(defn create-card
  "Create a new card."
  [question answer]
  (atom {:question question 
         :answer answer
         :easy-factor 2.5
         :last-review-date nil
         :next-review-date nil
         :high-quality-reviews 0}))

(defn next-easy-factor [last-easy-factor recall]
  (+ last-easy-factor (- 0.1 (* (- 5 recall) (+ 0.08 (* (- 5 recall) 0.02))))))

(defn review-card! [card recall]
  (swap! card update-in [:easy-factor] next-easy-factor recall))

(defn next-review-interval
  "Determine how many days to review the card within."
  [card]
  (let [{:keys [easy-factor schedule]} @card]
    (cond 
     (= (count schedule) 0) 1
     (= (count schedule) 1) 6
     :else (int (* (:review-within-days (last schedule)) easy-factor)))))

(defn review-card [card recall]
  (let [card @card]
    (update-in card [:easy-factor] next-easy-factor recall)
    (update-in card [:schedule] conj {:from (local-now) 
                                      :review-within-days (next-review-interval)})))

(defn review-card-by-date
  "Determine the date by which the card should be reviewed."
  [card]
  (clj-time/plus (:from (last (:schedule @card))) (clj-time/days (:review-within-days (last (:schedule @card))))))


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


(comment
  (defn cards-to-review
    "Return the collection of cards that need to be reviewed today"
    [deck date]
    (filter (partial date review-card?) deck)))

(defn review-card? [date card]
  (let [{:keys [next-review-date]} @card]
    (if (nil? next-review-date) true
        (clj-time/before? next-review-date date))))

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
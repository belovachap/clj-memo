(ns clj-memo.core-spec
  (:require [speclj.core :refer :all]
            [clj-time.core :refer [date-midnight plus minus days]]
            [clj-memo.core :refer :all]))

(describe "review-card?"

  (it "should be true if the card has never been reviewed"
    (let [card (atom {:next-review-date nil})]
      (should (review-card? (date-midnight 200 1 1) card))))

  (it "should be true if the card is overdue for review"
    (let [card (atom {:next-review-date (date-midnight 1999 12 1)})]
      (should (review-card? (date-midnight 2000 1 1) card))))

  (it "should be true if the next review date is today"
    (let [card (atom {:next-review-date (date-midnight 2000 1 1)})]
      (should (review-card? (date-midnight 2000 1 1) card))))

  (it "should be false if the next review date is in the future"
    (let [card (atom {:next-review-date (date-midnight 2000 1 2)})]
      (should-not (review-card? (date-midnight 2000 1 1) card)))))

(defn random-date-on-or-before [date]
  (minus date (days (rand-int 10))))

(defn random-date-after [date]
  (plus date (days (inc (rand-int 10)))))

(defn card-with-next-review-date [date date-fn]
  (atom {:next-review-date (date-fn date)}))

(describe "cards-to-review"

  (it 
    "should return a sequence of cards that need to be reviewed"
    (let [date (date-midnight 2000 1 1)
          review-cards (repeatedly 2 (partial card-with-next-review-date date random-date-on-or-before))
          not-review-cards (repeatedly 2 (partial card-with-next-review-date date random-date-after))
          all-cards (concat review-cards not-review-cards)]
      (should== review-cards (cards-to-review date all-cards)))))

(run-specs)
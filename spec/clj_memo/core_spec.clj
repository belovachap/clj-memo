(ns clj-memo.core-spec
  (:require [speclj.core :refer :all]
            [clj-time.core :refer [date-midnight plus minus days]]
            [clj-memo.core :refer :all]))


(describe "update-high-quality-reviews"

  (it "should return 0 if the recall is less than 3"
    (let [high-quality-reviews 5
          recall 2]
      (should= 0 (update-high-quality-reviews high-quality-reviews recall))))
  
  (it "should increment the current number of high quality reviews if recall is 3 or better"
    (let [high-quality-reviews 5
          recall 3]
      (should= 6 (update-high-quality-reviews high-quality-reviews recall)))))


(describe "update-easy-factor"

  (it "should recompute the easy factor if recall is 3 or better"
    (let [easy-factor 2.5
          recall 3]
      (should= 2.36 (update-easy-factor easy-factor recall))))

  (it "should not change the easy factor if the recall is less than 3"
    (let [easy-factor 2.5
          recall 2]
      (should= 2.5 (update-easy-factor easy-factor recall)))))

(describe "update-review-interval"
  
  (it "should return 1 if high quality reviews is 0"
    (should= 1 (update-review-interval 1 0 2.5)))

  (it "should return 6 if high quality reviews is 1"
    (should= 6 (update-review-interval 1 1 2.5)))

  (it "should return SM2 computation for more than 2 high quality reviews"
    (should= 3 (update-review-interval 1 2 2.5))))

(describe "last-review-interval"
  
  (it "should return nil if last-review-date is nil"
    (should= nil (last-review-interval nil nil)))

  (it "should return the difference in days of two dates"
    (let [date1 (date-midnight 2000 1 1)
          date2 (date-midnight 2000 1 3)]
      (should= 2 (last-review-interval date1 date2)))))

(describe "review-card?"

  (it "should be true if the card has never been reviewed"
    (let [card (atom {:next-review-date nil})]
      (should (review-card? (date-midnight 2000 1 1) card))))

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
(ns clj-memo.core-spec
  (:require [speclj.core :refer :all]
            [clj-time.core :as clj-time]
            [clj-memo.core :refer :all]))

(defn add-to-schedule [card review-window]
  (swap! card update-in [:schedule] conj review-window))

(describe "card"
  (it "should have a question, answer, schedule and easy factor"
    (let [new-card @(create-card "question" "answer")]
      (should= "question" (:question new-card))
      (should= "answer" (:answer new-card))
      (should= 2.5 (:easy-factor new-card))
      (should= [] (:schedule new-card))))
  
  (it "should know when it's due for review"
    (let [card (create-card "q" "a")]
      (add-to-schedule card {:from (clj-time/date-time 2000 1 1) :review-within-days 1})
      (should= (clj-time/date-time 2000 1 2) (review-card-by-date card))))

  (it "should update when reviewed"
    (let [card (atom {:easy-factor 2.5 :schedule []})
          reviewed-card (review-card! card 3)]
      (should= 2.36 (:easy-factor reviewed-card)))
    )
  )

(describe "Training program"
  )

(describe "SuperMemo2 card scheduling"
  (it "should schedule a new card to be reviewed within a day"
    (let [new-card (create-card "question" "answer")]
      (should= 1 (next-review-interval new-card))))

  (it "should schedule the second review within six days"
    (let [card (create-card "question" "answer")]
      (add-to-schedule card {:from (clj-time/date-time 2000 1 1) :review-within-days 1})
      (should= 6 (next-review-interval card))))

  (it "should use the SM2 update equation from iteration 3 on"
    (let [card (create-card "question" "answer")]
      (add-to-schedule card {:from (clj-time/date-time 2000 1 1) :review-within-days 1})
      (add-to-schedule card {:from (clj-time/date-time 2000 1 1) :review-within-days 6})
      (should= 15 (next-review-interval card)))))

(describe "Card deck operations"
  (it "should be able to get the next scheduled card"
    (let 
        [soon-card (create-card  "q" "a")
         later-card (create-card "q" "a")
         deck [soon-card later-card]]
      (add-to-schedule soon-card {:from (clj-time/date-time 2000 1 1) :review-within-days 1})
      (add-to-schedule later-card {:from (clj-time/date-time 2000 1 1) :review-within-days 6})

      (should= soon-card (next-card-to-review deck)))))

(run-specs)
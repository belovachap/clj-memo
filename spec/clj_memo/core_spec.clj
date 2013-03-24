(ns clj-memo.core-spec
  (:require [speclj.core :refer :all]
            [clj-time.core :as clj-time]
            [clj-memo.core :refer :all]))

(describe "card"
  (it "should have a question, answer, schedule and easy factor"
    (let [new-card (create-card "question" "answer")]
      (should= "question" (:question new-card))
      (should= "answer" (:answer new-card))
      (should= 2.5 (:easy-factor new-card))
      (should= [] (:schedule new-card))))
  
  (it "should know when it's due for review"
    (let [card (assoc (create-card "q" "a") :schedule [{:from (clj-time/date-time 2000 1 1)
                                                         :review-within-days 1}])]
      (should= (clj-time/date-time 2000 1 2) (review-card-by-date card))))
)

(describe "SuperMemo2 card scheduling"
  (it "should schedule a new card to be reviewed within a day"
    (let [new-card (create-card "question" "answer")]
      (should= 1 (review-card-within-days new-card))))

  (it "should schedule the second review within six days"
    (let [card (create-card "question" "answer")]
      (should= 6 (review-card-within-days 
                  (assoc card :schedule [{:from "start-time"
                                          :review-within-days 1}])))))

  (it "should use the SM2 update equation from iteration 3 on"
    (let [card (create-card "question" "answer")
          card (assoc card :schedule [{:from "start-time"
                                       :review-within-days 1}
                                      {:from "start-time"
                                       :review-within-days 6}])]
      (should= 15 (review-card-within-days card)))))

(describe "Card deck operations"
  (it "should be able to get the next scheduled card"
    (let 
        [soon-card (assoc (create-card  "q" "a") :schedule [{:from (clj-time/date-time 2000 1 1)
                                                             :review-within-days 1}])
         later-card (assoc (create-card "q" "a") :schedule [{:from (clj-time/date-time 2000 1 1)
                                                             :review-within-days 2}])
         deck [soon-card later-card]]
      (should= soon-card (next-card-to-review deck)))))

(run-specs)
(ns clj-memo.core-spec
  (:require [speclj.core :refer :all]
            [clj-memo.core :refer :all]))

(defn true-or-false []
  true)

(describe "card"
  (it "should have a question, answer, schedule and easy factor"
    (let [new-card (create-card "question" "answer")]
      (should= "question" (:question new-card))
      (should= "answer" (:answer new-card))
      (should= 2.5 (:easy-factor new-card))
      (should= [] (:schedule new-card)))))

(describe "SuperMemo2 card scheduling"
  (it "should schedule a new card to be reviewed within a day"
    (let [new-card (create-card "question" "answer")]
      (should= 1 (review-card-within-days new-card)))))

(run-specs)
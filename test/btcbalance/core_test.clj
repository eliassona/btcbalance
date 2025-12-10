(ns btcbalance.core-test
  (:require [clojure.test :refer :all]
            [btcbalance.core :refer :all]))

(deftest test-stock-split
  (is (= [[10 {:coke {:usd 1}}]] 
         (stock-split [[10 {:coke {:usd 10}}]] :coke 10 0)))
  (is (= [[10 {:coke {:usd 10}}] [9 {:coke {:usd 10}}]] 
         (stock-split [[10 {:coke {:usd 10}}][9 {:coke {:usd 100}}]] :coke 10 1)))
  (is (= [[10 {:coke {:usd 10, :sek 1000}}] [9 {:coke {:usd 10, :sek 100}}]] 
         (stock-split [[10 {:coke {:usd 10, :sek 1000}}][9 {:coke {:usd 100, :sek 1000}}]] :coke 10 1)))
  
  )

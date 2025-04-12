(ns btcbalance.quantity-test
  (:require [clojure.test :refer :all]
            [btcbalance.quantity :refer :all]))

(def cm 
  (inv-of 
    {:btc {:usd (fn [] 1000)
           :sek (fn [] 10000)
           :eur (fn [] 500)
           :jpy (fn [] 100000)
           :chf (fn [] 700)
               }
     :gold {:usd (fn [] 5)}
         }))

(deftest test-path
  (is (= [:usd :btc] (unit-path-of cm :usd :btc)))
  (is (= [:btc :usd] (unit-path-of cm :btc :usd)))
  (is (= [:sek :btc :usd] (unit-path-of cm :sek :usd)))
  (is (= [:usd :btc :sek ] (unit-path-of cm :usd :sek)))
  (is (= [:btc :usd :gold] (unit-path-of cm :btc :gold)))
  )

(deftest test_convert
  (is (= (quantity 1000 :usd) (convert cm (quantity 1 :btc) :usd)))
  (is (= (quantity 1 :btc) (convert cm (quantity 1000 :usd) :btc)))
  (is (= (quantity 1 :btc) (convert cm (quantity 10000 :sek) :btc)))
  (is (= (quantity 5 :usd) (convert cm (quantity 1 :gold) :usd)))
  (is (= (quantity 1 :gold) (convert cm (quantity 5 :usd) :gold)))
  (is (= (quantity 1/200 :btc) (convert cm (quantity 1 :gold) :btc)))
  (is (= (quantity 200 :gold) (convert cm (quantity 1 :btc) :gold)))
  ) 
(deftest test-rate
  (is (= 1000 (rate cm :btc :usd)))
  (is (= 1/1000 (rate cm :usd :btc)))
  )
(deftest test-add
  (is (= (quantity 2 :btc) (add cm (quantity 1 :btc) (quantity 1 :btc))))
  (is (= (quantity 10001 :sek) (add cm (quantity 1 :sek) (quantity 1 :btc))))
  (is (= (quantity 0 :btc) (sub cm (quantity 1 :btc) (quantity 1 :btc))))
  (is (= (quantity -9999 :sek) (sub cm (quantity 1 :sek) (quantity 1 :btc))))
  (is (= (quantity 1 :btc) (mul (quantity 1 :btc) 1)))
  (is (= (quantity 2 :sek) (mul (quantity 1 :sek) 2)))
  (is (= (quantity 1 :btc) (div (quantity 1 :btc) 1)))
  (is (= (quantity 2 :sek) (div (quantity 10 :sek) 5)))
  )
  


(ns btcbalance.quantity-test
  (:require [clojure.test :refer :all]
            [btcbalance.quantity :refer :all]))
(set-conversion-map! 
  {:btc {:usd (fn [] 1000)
             :sek (fn [] 10000)
             :eur (fn [] 500)
             :jpy (fn [] 100000)
             :chf (fn [] 700)
             }
   :gold {:usd (fn [] 5)}
       })

(deftest test-path
  (is (= [:usd :btc] (unit-path-of :usd :btc)))
  (is (= [:btc :usd] (unit-path-of :btc :usd)))
  (is (= [:sek :btc :usd] (unit-path-of :sek :usd)))
  (is (= [:usd :btc :sek ] (unit-path-of :usd :sek)))
  )

(deftest test_convert
  (is (= (->Quantity 1000 :usd) (convert (->Quantity 1 :btc) :usd)))
  (is (= (->Quantity 1 :btc) (convert (->Quantity 1000 :usd) :btc)))
  (is (= (->Quantity 1 :btc) (convert (->Quantity 10000 :sek) :btc)))
  (is (= (->Quantity 5 :usd) (convert (->Quantity 1 :gold) :usd)))
  (is (= (->Quantity 1 :gold) (convert (->Quantity 5 :usd) :gold)))
  ) 
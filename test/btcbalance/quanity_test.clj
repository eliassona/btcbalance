(ns btcbalance.quantity-test
  (:require [clojure.test :refer :all]
            [btcbalance.quantity :refer :all]))

(def rates 
  (inv-of 
    {:btc {:usd (fn [] 1000)
           :sek (fn [] 10000)
           :eur (fn [] 500)
           :jpy (fn [] 100000)
           :chf (fn [] 700)
               }
     :gold {:usd (fn [] 5)}
         }))

(deftest test-valid-units
  (is (= (into #{} (keys rates)) (unit-set rates))))

(deftest test-path
  (is (= [:usd :btc] (unit-path-of rates :usd :btc)))
  (is (= [:btc :usd] (unit-path-of rates :btc :usd)))
  (is (= [:sek :btc :usd] (unit-path-of rates :sek :usd)))
  (is (= [:usd :btc :sek ] (unit-path-of rates :usd :sek)))
  (is (= [:btc :usd :gold] (unit-path-of rates :btc :gold)))
  )

(deftest test-convert
  (is (= (q 1 :btc) (convert rates (q 1 :btc) :btc)))
  (is (= (q 1000 :usd) (convert rates (q 1 :btc) :usd)))
  (is (= (q 1 :btc) (convert rates (q 1000 :usd) :btc)))
  (is (= (q 1 :btc) (convert rates (q 10000 :sek) :btc)))
  (is (= (q 5 :usd) (convert rates (q 1 :gold) :usd)))
  (is (= (q 1 :gold) (convert rates (q 5 :usd) :gold)))
  (is (= (q 1/200 :btc) (convert rates (q 1 :gold) :btc)))
  (is (= (q 200 :gold) (convert rates (q 1 :btc) :gold)))
  ) 
(deftest test-rate
  (is (= 1000 (rate rates :btc :usd)))
  (is (= 1/1000 (rate rates :usd :btc)))
  )
(deftest test-add-sub-mul-div
  (is (= (q 2 :btc) (add rates (q 1 :btc) (q 1 :btc))))
  (is (= (q 10001 :sek) (add rates (q 1 :sek) (q 1 :btc))))
  (is (= (q 0 :btc) (sub rates (q 1 :btc) (q 1 :btc))))
  (is (= (q -9999 :sek) (sub rates (q 1 :sek) (q 1 :btc))))
  (is (= (q 1 :btc) (mul (q 1 :btc) 1)))
  (is (= (q 4 :sek) (mul (q 1 :sek) 2 2)))
  (is (= (q 1 :btc) (div (q 1 :btc) 1)))
  (is (= (q 2 :sek) (div (q 10 :sek) 5)))
  )
  
(deftest test-comparators
  (is (= true (gt rates (q 2 :sek) (q 1 :sek))))
  (is (= false (gt rates (q 1 :sek) (q 2 :sek))))
  (is (= true (gt rates (q 1 :btc) (q 2 :sek))))
  
  )

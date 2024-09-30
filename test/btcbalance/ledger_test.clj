(ns btcbalance.ledger-test
  (:require [clojure.test :refer :all]
            [btcbalance.ledger :refer :all])
  (:import [java.util Date]))

(def test-dir (-> "testdata" clojure.java.io/resource clojure.java.io/file))
(def the-data 
  [
   {:sek 290.8125116325, :sats 37000, :sek->btc 785979.7611689189, :date #inst "2024-06-07T18:01:03.000-00:00", :exchange :trijo, :id "O-AP-DB456772"} 
   {:sek 290.8124883675, :sats 36900, :sek->btc 788109.7245731706, :date #inst "2024-06-08T18:02:03.000-00:00", :exchange :trijo, :id "O-AP-7149CB8C"}
   {:sek 290.8124883675, :sats 36800, :sek->btc 790251.3270855979, :date #inst "2024-06-10T18:04:03.000-00:00", :exchange :trijo, :id "O-AP-EF73235C"}
   {:sek 290.8125116325, :sats 36800, :sek->btc 790251.3903057065, :date #inst "2024-06-09T18:03:02.000-00:00", :exchange :trijo, :id "O-AP-54D18898"}
   {:sek 288.125, :sats 36400, :sek->btc 791552.1978021978, :date #inst "2024-04-11T17:06:02.000-00:00", :exchange :trijo, :id "O-AP-F3C706B3"}
   {:sek 290.8124651025, :sats 36700, :sek->btc 792404.5370640329, :date #inst "2024-06-04T17:59:03.000-00:00", :exchange :trijo, :id "O-AP-9830717A"}
   {:sek 290.8125, :sats 36700, :sek->btc 792404.6321525886, :date #inst "2024-06-06T18:01:02.000-00:00", :exchange :trijo, :id "O-AP-A42B36C3"}
   {:sek 288.124988475, :sats 36100, :sek->btc 798130.1619806094, :date #inst "2024-03-31T16:56:03.000-00:00", :exchange :trijo, :id "O-AP-D74E2FC9"}
   {:sek 290.8124883675, :sats 36300, :sek->btc 801136.3315909092, :date #inst "2024-06-05T18:00:04.000-00:00", :exchange :trijo, :id "O-AP-2B34368F"}
   {:sek 288.124988475, :sats 35900, :sek->btc 802576.5695682452, :date #inst "2024-04-08T17:03:03.000-00:00", :exchange :trijo, :id "O-AP-CDB303DA"}
   ])
  
(defn filter-spend [m]
  {:id (:id m)
   :sats (:sats m)
   :sek->btc (:sek->btc m)})
(defn find-to-spend [sats]
  (map filter-spend (find-tx-to-spend sats)))               
                 
  
(deftest test-last-full
  #_(binding [dir test-dir] 
     (is (= [{:id "O-AP-CDB303DA", :sats 35900, :sek->btc 802576.5695682452}] 
            (find-to-spend 35900))))
  )
  
(deftest test-last-full-and-one-sat-of-second-last
  #_(binding [dir test-dir] 
     (is (= [
             {:id "O-AP-CDB303DA", :sats 35900, :sek->btc 802576.5695682452}
             {:id "O-AP-2B34368F", :sats 1, :sek->btc 801136.3315909092}
             ] 
            (find-to-spend 35901)))
     (is (= [
             {:id "O-AP-CDB303DA", :sats 35900, :sek->btc 802576.5695682452}
             {:id "O-AP-2B34368F", :sats 2, :sek->btc 801136.3315909092}
             ] 
            (find-to-spend 35902)))
     )
  )
  
(deftest test-calc-profit
  (binding [dir test-dir] 
    (is 
      (= 
        [{:profit -68.81251163249999, :value 222.0} 
         {:profit -69.4124883675, :value 221.4} 
         {:profit -70.0124883675, :value 220.8} 
         {:profit -70.01251163249998, :value 220.8} 
         {:profit -69.725, :value 218.4} 
         {:profit -70.61246510250004, :value 220.2} 
         {:profit -70.61250000000001, :value 220.2} 
         {:profit -71.52498847500001, :value 216.6} 
         {:profit -73.01248836750003, :value 217.79999999999998} 
         {:profit -72.724988475, :value 215.4}] 
        (calc-profit 600000 the-data))) 
  ))
(deftest test_testdata
  (binding [dir test-dir]
    (let [_ (reset! data [])
          t-o (total-orders)
          t-b (total-balance)
          ]
    (is (= 16 (count t-o)))
    (is (= ["safello6" "safello5" "safello3" "safello2" "safello4" "1" "2" "3" "4" "6" "5" "7" "trijo3" "trijo5" "trijo4" "trijo6"] (map :id t-o)))
    (is (= t-b t-o))
    (is (= [{:sek 10000, :sats 13524660, :sek->btc 73939.01214522214, :date #inst "2020-04-12T08:15:34.000-00:00", :exchange :safello, :id "safello6"}]
           (find-tx-to-spend 13524660)))
    (is (= [{:sek 10000, :sats 13524660, :sek->btc 73939.01214522214, :date #inst "2020-04-12T08:15:34.000-00:00", :exchange :safello, :id "safello6"}
            {:sek 10000, :sats 1, :sek->btc 75022.35103393179, :date #inst "2020-04-20T18:20:16.000-00:00", :exchange :safello, :id "safello5"}]
           (find-tx-to-spend 13524661)))
    (let [date (Date.)]
      (doseq [tx (find-tx-to-spend 13524661)]
        (spend! tx date)
        ))
    (is (= [["safello6" 13524660 73939.01214522214] ["safello5" 1 75022.35103393179]] @data))
    (is (= ["safello5" "safello3" "safello2" "safello4" "1" "2" "3" "4" "6" "5" "7" "trijo3" "trijo5" "trijo4" "trijo6"] 
           (map :id (total-balance))))
    (is (= 13329360 (:sats (get (total-balance-map) "safello5"))))
    )))
  








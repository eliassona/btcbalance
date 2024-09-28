(ns btcbalance.ledger-test
  (:require [clojure.test :refer :all]
            [btcbalance.ledger :refer :all]))

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
  (is (= [{:id "O-AP-CDB303DA", :sats 35900, :sek->btc 802576.5695682452}] 
         (find-to-spend 35900))))

(deftest test-last-full-and-one-sat-of-second-last
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








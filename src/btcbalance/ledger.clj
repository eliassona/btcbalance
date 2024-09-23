(ns btcbalance.ledger
  (:require [btcbalance.core :refer [growth dbg]]
           [bchain.core :refer [EUR-last SEK SEK-last USD USD-last satoshi]]
           [clojure.string :as str]
           [clojure.data.json :as json]
           )
  (:import [java.util Date]
           [java.io File]
           [java.time OffsetDateTime LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]))

(defn sek-usd-rate [] (/ (SEK-last) (USD-last)))
(defn split-csv [csv-string]
  (json/read-str (str "[" csv-string "]")))

(def dir (File. ""))

(defn btcx-value-date [values]
  (let [sek (read-string (first values))
        btc (read-string (read-string (nth values 7)))]
  {:sek sek
   :btc btc
   :sek->btc (double (/ sek btc))
   :date (Date. (read-string (nth values 14)))
   :network-fee (nth values 13)}))

(defn btcx-orders []
  (map btcx-value-date
    (filter (fn [v] (= (nth v 20) "COMPLETED"))    
      (map 
        #(.split % ",") 
        (rest (.split (slurp (File. dir "btcx.csv")) "\n")))
  )))

(defn string-to-date [date-string]
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ssZ")
        offset-date-time (OffsetDateTime/parse date-string formatter)]
    (Date/from (.toInstant offset-date-time))))

(defn safello-value-date [values]
  (let [sek (read-string (nth values 7))
        btc (read-string (nth values 8))]
  {:sek sek
   :btc btc
   :sek->btc (double (/ sek btc))
   :date (string-to-date (nth values 6))
  }))

(defn safello-orders []
  (map
    safello-value-date
    (filter 
      #(= (nth % 5) "completed")
      (map 
        #(.split % ";")
        (rest (.split (slurp (File. dir "safello.csv")) "\n"))))))


(defn eur->sek [v]
  (* v (/ (SEK-last) (EUR-last))))

(defn trijo-string-to-date [date-string]
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")
        local-date-time (LocalDateTime/parse date-string formatter)
        zone-id (ZoneId/systemDefault)]  ;; You can change this to another timezone if needed
    (Date/from (.toInstant (.atZone local-date-time zone-id)))))

(defn trijo-value-date [values]
  (let [sek (eur->sek (read-string (read-string (nth values 7)))) ;TODO get eur->sek rate for this date
        btc (read-string (read-string (nth values 4)))]
  {:sek sek
   :btc btc
   :sek->btc (double (/ sek btc))
   :date (trijo-string-to-date (read-string (nth values 1)))
  }))

(def trijo-files (filter #(.startsWith (.getName %) "trijo") (.listFiles dir)))

(defn trijo-orders-of [filename]
  (map
    trijo-value-date
    (filter 
      #(and (= (read-string (nth % 2)) "ordrar") (= (read-string (nth % 8)) "GENOMFÖRD"))
      (map 
        #(.split % ",")
        (rest (.split (slurp filename) "\n"))))))

(defn trijo-orders []
  (apply concat (map trijo-orders-of trijo-files)))

(defn total-orders []
  (apply concat [(trijo-orders) (safello-orders) (btcx-orders)]))

(defn total-sek-spent [] (apply + (map :sek (total-orders))))

(defn total-btc [] (apply + (map :btc (total-orders))))

(defn total-sek [] (* (SEK-last) (total-btc)))



(defn trijo-deposit-date [values]
  {
   :date (trijo-string-to-date (nth values 1))
   :eur (read-string (apply str (.split (nth values 4) ",")))
   })



(defn deposit? [values]
  (and 
    (= (nth values 2) "INSÄTTNING") 
    (= (nth values 7) "GENOMFÖRD")))

(defn trijo-deposits-of [filename]
  (let [values (map #(split-csv % ) (rest (.split (slurp filename) "\n")))
        values (filter deposit? values)]
    (map
      trijo-deposit-date
        values)))

(defn trijo-deposits []
  (apply concat (map trijo-deposits-of trijo-files)))


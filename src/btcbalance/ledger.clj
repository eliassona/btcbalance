(ns btcbalance.ledger
  (:require [btcbalance.core :refer [growth dbg]]
           [bchain.core :refer [EUR-last SEK SEK-last USD USD-last satoshi]]
           [clojure.string :as str]
           [clojure.data.json :as json]
           [clojure.xml :as xml]
           )
  (:import [java.util Date]
           [java.text SimpleDateFormat]
           [java.io File]
           [java.time OffsetDateTime LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [btcbalance HistoricalRate]))
(def dir (File. (System/getProperty "user.home") "btcorders"))

;https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/eurofxref-graph-sek.en.html
(def eur->sek-historical-rates-map
  (into 
    {}
    (let [formatter (SimpleDateFormat. "yyyy-MM-dd")]
      (map 
        (fn [m] 
          (let [attrs (:attrs m)]
            [(:TIME_PERIOD attrs)
            (read-string (:OBS_VALUE attrs))]
             ))
        (:content (second (:content (second (:content (xml/parse (File. dir "eur-sek.xml")))))))))))


(defn sek-usd-rate [] (/ (SEK-last) (USD-last)))
(defn split-csv [csv-string]
  (json/read-str (str "[" csv-string "]")))


;btcx orders

(defn btcx-value-date [values]
  (let [sek (read-string (first values))
        btc (read-string (read-string (nth values 7)))]
  {:sek sek
   :btc btc
   :sek->btc (double (/ sek btc))
   :date (Date. (read-string (nth values 14)))
   :network-fee (nth values 13)
   :exchange :btcx
   :id (nth values 14)
   }))

(defn btcx-orders []
  (map btcx-value-date
    (filter (fn [v] (= (nth v 20) "COMPLETED"))    
      (map 
        #(.split % ",") 
        (rest (.split (slurp (File. dir "btcx.csv")) "\n")))
  )))


;safello orders

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
   :exchange :safello
   :id (first values)
  }))

(defn safello-orders 
  ([filename]
  (map
    safello-value-date
    (filter 
      #(= (nth % 5) "completed")
      (map 
        #(.split % ";")
        (rest (.split (slurp (File. dir filename)) "\n"))))))
  ([] (safello-orders "safello.csv")))


;trijo deposits

(def date-formatter (SimpleDateFormat. "yyyy-MM-dd"))

(defn date->string [date]
  (.format date-formatter date))

(defn trijo-string-to-date [date-string]
  (let [formatter (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm:ss")
        local-date-time (LocalDateTime/parse date-string formatter)
        zone-id (ZoneId/systemDefault)]  ;; You can change this to another timezone if needed
    (Date/from (.toInstant (.atZone local-date-time zone-id)))))


(defn parse-value-with-commas [v]
  (read-string (apply str (.split v ","))))

(defn trijo-deposit-date [values]
  {
   :date (trijo-string-to-date (nth values 1))
   :eur (parse-value-with-commas (nth values 4))
   })

(defn deposit? [values]
  (and 
    (= (nth values 2) "INSÄTTNING") 
    (= (nth values 7) "GENOMFÖRD")))

(def trijo-files (filter #(.startsWith (.getName %) "trijo") (.listFiles dir)))
(def trijo-split-values (apply 
                          concat 
                          (map #(map split-csv (rest (.split (slurp %) "\n"))) trijo-files)))

(defn trijo-deposits-of [values]
  (let [values (filter deposit? values)]
    (map trijo-deposit-date values)))


(def trijo-deposits
  (sort-by :date (trijo-deposits-of trijo-split-values)))

(def depo-date (HistoricalRate. trijo-deposits))  


(defn deposit-date-of [order-date]
  (let [order-date-in-ms (.getTime order-date)]
    (loop [
           depo (-> trijo-deposits first :date)
           rest-deposits (rest trijo-deposits)]
      (if-let [curr (-> rest-deposits first :date)]
        (if (> order-date-in-ms (.getTime curr))
          (recur curr (rest rest-deposits))
          depo)
        depo))))

(defn str->date [date-str]
  (let [date-formatter (SimpleDateFormat. "yyyy-MM-dd")]
    (.parse date-formatter date-str)))

(def day-in-ms (* 24 3600 1000))

(defn eur->sek-rate-of [date]
  (if-let [rate (get eur->sek-historical-rates-map (date->string date))]
    rate
    (eur->sek-rate-of (Date. (- (.getTime date) day-in-ms)))))

(defn eur->sek ([v]
  (* v (/ (SEK-last) (EUR-last))))
  ([v date]
    (* v (eur->sek-rate-of (deposit-date-of date)))
    #_(eur->sek v)))

;trijo orders





(defn trijo-value-date [values]
  (let [
        date (trijo-string-to-date (nth values 1))
        sek (eur->sek (parse-value-with-commas (nth values 6)) date) 
        btc (read-string (nth values 4))
        ]
    
  {
   :sek sek
   :btc btc
   :sek->btc (double (/ sek btc))
   :date date
   :exchange :trijo
   :id (first values)
  }))


(defn trijo-orders-of [values]
  (map
    trijo-value-date
    (filter 
      #(and (= (nth % 2) "ordrar") (= (nth % 7) "GENOMFÖRD"))
      values)))

(defn trijo-orders []
  (sort-by :date (trijo-orders-of trijo-split-values)))

(defn foreign-orders []
  (safello-orders "foreign.csv"))


(def total-orders 
  (sort-by :sek->btc (apply concat [(trijo-orders) (safello-orders) (btcx-orders)])))

(defn total-sek-spent [] (apply + (map :sek total-orders)))

(defn total-btc [] (apply + (map :btc total-orders)))

(defn total-sek [] (* (SEK-last) (total-btc)))

(defn calc-profit-loss [curr-sek->btc-rate m]
  (let [btc (:btc m)
        rate (:sek->btc m)]
    (- (* btc curr-sek->btc-rate) (* btc rate))))

(defn get-txs-in-loss []
  (let [the-fn (partial calc-profit-loss (SEK-last))]
    (filter (fn [m] (neg? (the-fn m))) total-orders)))

(defn txs-in-profit-of [value-in-btc]
  (loop [sum-in-btc 0
         orders-in-profit []
         values total-orders]
    (if (and (< sum-in-btc value-in-btc) (not (empty? values)))
      (recur (+ sum-in-btc (-> values first :btc)) 
             (conj orders-in-profit (first values)) 
             (rest values))
      orders-in-profit)))



(assert (= (count (set (map :id total-orders))) (count total-orders)) "Error, ids are not unique")

(def t-file (clojure.java.io/resource "t.txt"))

(def data (atom (read-string (slurp t-file))))

(defn save! []
  (spit t-file (pr-str @data)))


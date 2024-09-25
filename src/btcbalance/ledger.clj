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
   :network-fee (nth values 13)}))

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


(defn trijo-deposit-date [values]
  {
   :date (trijo-string-to-date (nth values 1))
   :eur (read-string (apply str (.split (nth values 4) ",")))
   })

(defn deposit? [values]
  (and 
    (= (nth values 2) "INSÄTTNING") 
    (= (nth values 7) "GENOMFÖRD")))

(def trijo-files (filter #(.startsWith (.getName %) "trijo") (.listFiles dir)))
(def trijo-split-values (map #(map split-csv (rest (.split (slurp %) "\n"))) trijo-files))

(defn trijo-deposits-of [values]
  (let [values (filter deposit? values)]
    (map trijo-deposit-date values)))


(defn trijo-deposits []
  (sort-by :date (apply concat (map trijo-deposits-of trijo-split-values))))

(def depo-date (HistoricalRate. (trijo-deposits)))  

(defn eur->sek ([v]
  (* v (/ (SEK-last) (EUR-last))))
  ([v date]
    (get eur->sek-historical-rates-map (date->string (.depoDateOf depo-date date)))
    (eur->sek v)))

;trijo orders





(defn trijo-value-date [values]
  (let [date (trijo-string-to-date (read-string (nth values 1)))
        sek (eur->sek (read-string (read-string (nth values 7))) date) 
        btc (read-string (read-string (nth values 4)))]
  {:sek sek
   :btc btc
   :sek->btc (double (/ sek btc))
   :date date
  }))


(defn trijo-orders-of [filename]
  (map
    trijo-value-date
    (filter 
      #(and (= (read-string (nth % 2)) "ordrar") (= (read-string (nth % 8)) "GENOMFÖRD"))
      (map 
        #(.split % ",")
        (rest (.split (slurp filename) "\n"))))))

(defn trijo-orders []
  (sort-by :date (apply concat (map trijo-orders-of trijo-files))))

(defn foreign-orders []
  (safello-orders "foreign.csv"))


(defn total-orders []
  (sort-by :date (apply concat [(foreign-orders) (trijo-orders) (safello-orders) (btcx-orders)])))

(defn total-sek-spent [] (apply + (map :sek (total-orders))))

(defn total-btc [] (apply + (map :btc (total-orders))))

(defn total-sek [] (* (SEK-last) (total-btc)))




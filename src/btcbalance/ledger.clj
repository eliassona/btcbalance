(ns btcbalance.ledger
  (:require [btcbalance.core :refer [growth dbg]]
           [bchain.core :refer [EUR-last SEK SEK-last USD USD-last satoshi]]
           [btcbalance.secure.pwd :as pwd]
           [clojure.string :as str]
           [clojure.data.json :as json]
           [clojure.xml :as xml]
           )
  (:import [java.util Date]
           [java.text SimpleDateFormat]
           [java.io File]
           [java.time OffsetDateTime LocalDateTime ZoneId]
           [java.time.format DateTimeFormatter]
           [btcbalance AES256]))
(def day-in-ms (* 24 3600 1000))

(def date-formatter (SimpleDateFormat. "yyyy-MM-dd"))

(defn str->date [date-str]
    (.parse date-formatter date-str))


(defn encrypt [master-key data]
  (->> data pr-str (AES256/encrypt master-key)))

(defn decrypt [master-key encrypted-data]
  (if master-key
    (try 
      (->> encrypted-data (AES256/decrypt master-key) read-string)
      (catch Exception e
        (throw (IllegalArgumentException. "wrong password"))))
    nil))

(def sats-per-btc 1e8)

(defn btc->sats [btc]
  (long (* btc sats-per-btc)))

(defn sats->btc [sats]
  (/ sats sats-per-btc))

(def two-weeks-in-ms (* 14 day-in-ms))

(defn up-to-date? [msg date]
  (let [curr-date (.getTime (Date.))]
    (when (< (+ two-weeks-in-ms (.getTime date)) curr-date)
      (println msg)))) 
    


(def ^:dynamic dir (-> "btcorders" clojure.java.io/resource clojure.java.io/file))

;https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/eurofxref-graph-sek.en.html
(def eur->sek-historical-rates-map
  (into 
    {}
    (map 
      (fn [m] 
        (let [attrs (:attrs m)]
          [(:TIME_PERIOD attrs)
          (read-string (:OBS_VALUE attrs))]
           ))
      (:content (second (:content (second (:content (xml/parse (File. dir "eur-sek.xml"))))))))))


(defn sek-usd-rate [] (/ (SEK-last) (USD-last)))
(defn split-csv [csv-string]
  (json/read-str (str "[" csv-string "]")))


;btcx orders

(defn btcx-value-date [values]
  (let [sek (read-string (first values))
        btc (read-string (read-string (nth values 7)))]
  {:sek sek
   :sats (btc->sats btc)
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
   :sats (btc->sats btc)
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

(defn trijo-files [] (filter #(.startsWith (.getName %) "trijo") (.listFiles dir)))
(defn trijo-split-values [] (apply 
                              concat 
                              (map #(map split-csv (rest (.split (slurp %) "\n"))) (trijo-files))))

(defn trijo-deposits-of [values]
  (let [values (filter deposit? values)]
    (map trijo-deposit-date values)))


(defn trijo-deposits []
  (sort-by :date (trijo-deposits-of (trijo-split-values))))

(defn deposit-date-of [order-date]
  (let [order-date-in-ms (.getTime order-date)
        td (trijo-deposits)]
    (loop [
           depo (-> td first :date)
           rest-deposits (rest td)]
      (if-let [curr (-> rest-deposits first :date)]
        (if (> order-date-in-ms (.getTime curr))
          (recur curr (rest rest-deposits))
          depo)
        depo))))



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
   :sats (btc->sats btc)
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
  (sort-by :date (trijo-orders-of (trijo-split-values))))


(defn foreign-orders []
  (safello-orders "foreign.csv"))

(defn total-orders [] 
  (sort-by :sek->btc (apply concat [(trijo-orders) (safello-orders) (btcx-orders)])))

(defn total-orders-as-map []
  (into {} (map (fn [values] [(:id values) (dissoc values :id)]) (total-orders))))
  

(defn total-sek-spent [] (apply + (map :sek (total-orders))))

(defn total-btc [] (sats->btc (apply + (map :sats (total-orders)))))

(defn total-sek [] (* (SEK-last) (total-btc)))

(defn calc-profit-loss [curr-sek->btc-rate m]
  (let [btc (sats->btc (:sats m))
        rate (:sek->btc m)]
    (- (* btc curr-sek->btc-rate) (* btc rate))))

(defn get-txs-in-loss []
  (let [the-fn (partial calc-profit-loss (SEK-last))]
    (filter (fn [m] (neg? (the-fn m))) (total-orders))))

(defn calc-tx-loss []
  (apply + (map (partial calc-profit-loss (SEK-last)) (get-txs-in-loss))))
(let [t-o (total-orders)]
  (assert (= (count (set (map :id t-o))) (count t-o)) "Error, ids are not unique"))

(def t-file (clojure.java.io/resource "t.txt"))

(def data (atom (decrypt pwd/pwd (slurp t-file))))

(defn save! []
  (spit t-file (encrypt pwd/pwd @data)))

(defn spend! 
  ([id sats sek-per-btc-rate date]
    (swap! data conj [id sats sek-per-btc-rate date]))
  ([{:keys [id sats sek->btc]} date]
    (spend! id sats sek->btc date)))

(defn total-balance-map [] 
  (loop [m (total-orders-as-map)
         b @data]
    (if-let [[id sats] (first b)]
      (let [t (get m id)
            v-in-sats (:sats t)
            new-balance (- v-in-sats sats)]
        (cond 
          (pos? new-balance)
          (recur (assoc m id (assoc t :sats (- v-in-sats sats))) (rest b))
          (zero? new-balance)
          (recur (dissoc m id) (rest b))      
          :else  
          (throw (IllegalStateException. (format "id %s is overspent" id)))))
      m)))


(defn total-balance []
  (let [tbm (total-balance-map)]
    (sort-by 
      :sek->btc 
      (map 
        (fn [id] (assoc (get tbm id) :id id)) 
        (keys tbm)))))

(defn find-tx-to-spend [sats-to-spend]
  (loop [orders (total-balance)
         sum-in-sats 0
         spent-orders []]
    (if-let [order (first orders)]
      (if (< sum-in-sats sats-to-spend)
        (let [sats (:sats order)
              new-sum-in-sats (+ sum-in-sats sats)]
          (recur (rest orders) 
                 new-sum-in-sats
                 (if (> new-sum-in-sats sats-to-spend)
                   (conj spent-orders (assoc order :sats (- sats (- new-sum-in-sats sats-to-spend))))
                   (conj spent-orders order))))
        spent-orders)
      (do
        (println "Could not fill order")
        spent-orders))))

(defn calc-profit [sek->btc-rate orders]
    (map 
      (fn [order]
        (let [btc (sats->btc (:sats order))
              sek (* sek->btc-rate btc)]
          {:profit (- sek (* (:sek->btc order) btc))
           :value sek
           })) orders))
(defn run-check []
  "Evaluate these to get a warning if the data is too old"
  (up-to-date? "Warning, the EUR to SEK historical exchange rate is more than two weeks old" (last (sort (map str->date (keys eur->sek-historical-rates-map)))))
  (up-to-date? "Warning, the last trijo order is more than two weeks old" (-> (trijo-orders) last :date))
  )

  
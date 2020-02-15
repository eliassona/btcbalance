(ns btcbalance.core
  (:use [clojure.pprint])
  (:require [bchain.core :refer [dbg rawaddr SEK SEK-last USD USD-last satoshi blocks historical-price]]
            [bchain.shape :as ss]
            [bchain.balance :refer [convert balance def-balance total-balance convert-to store! all-balance store-proc]]
            )
  (:import [java.net URL]
           [java.text SimpleDateFormat]))

(defn url-get [url]
  (let [con (.openConnection (URL. url))
        _ (.setRequestMethod con "GET")
        in (.getInputStream con)
        ]
    (try
      (slurp in)
      (finally (.close in)))))


(defn growth [money interest days]
  (reduce (fn [acc _] (* acc interest)) money (range days)))

(defn parse-price [p]
  (read-string (reduce str (.split p ","))))

(defn parse-date [d]
  d)

(def date-template (SimpleDateFormat. "MMM dd, yyyy"))

(defn date-str->unix-time [d] (.parse date-template d))

(defn parse-it [[date open high low close volume market-cap]]
  [:date (parse-date date), 
   :open (parse-price open), 
   :high (parse-price high), 
   :low (parse-price low), 
   :close (parse-price close), 
   :volume (parse-price volume), 
   :market-cap (parse-price market-cap)]
  )





(defn split-it [v]
  (apply hash-map (parse-it (.split v "\t"))))

(def raw-btc-price 
    (map split-it 
         (.split (slurp (clojure.java.io/resource "btcprice")) "\n")))


(defn- latest-time []
  (.getTime (date-str->unix-time (:date (first raw-btc-price)))))

(defn- current-time [] (System/currentTimeMillis))

(def day-in-millis (* 24 3600 1000))

(defn- days-missing [] (inc (int (/ (- (current-time) (latest-time)) day-in-millis))))

(defn make-compatible [m]
  (let [t (:time m)
        p (:price m)]
    {:open p, :date t, :market-cap 0, :close p, :volume 0, :high p, :low p}))
  

(def raw-btc-price(concat (map make-compatible (historical-price (days-missing))) raw-btc-price))

(defn last-of [ix n]
  (take n (drop ix raw-btc-price)))


(defn moving-average-of [ix n] 
  (/ (reduce + (map :close (last-of ix n))) n))


(defn mayer-multiple-of [ix] (/ (-> raw-btc-price (nth ix) :close) (moving-average-of ix 200)))

(def mayer-multiples 
  (map (fn [ix] {:date (-> raw-btc-price (nth ix) :date) :mm (mayer-multiple-of ix)}) (range (- (count raw-btc-price) 200))))

(defn average-multiple []
  (/ (reduce + (map :mm mayer-multiples)) (count mayer-multiples)))


(defn buy-btc? [] (< (mayer-multiple-of 0) (average-multiple)))
  

(defn growth [value percent n]
  (let [pc (+ 1 (/ percent 100))]
    (loop [value value
           n n]
      (if (> n 0)
        (recur (* pc value) (dec n))
        (double value)))))

(String/format "You should %s buy btc" (if (buy-btc?) "" "not"))


(ns btcbalance.core
  (:use [clojure.pprint])
  #_(:require [bchain.core :refer [dbg rawaddr SEK SEK-last USD USD-last satoshi blocks historical-price]]
             [bchain.shape :as ss]
             [bchain.balance :refer [convert balance def-balance total-balance convert-to store! all-balance store-proc]]
            
             )
  (:import [java.net URL]
           [java.text SimpleDateFormat]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

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
  
(defn growth [value percent n]
  (let [pc (+ 1 (/ percent 100))]
    (loop [value value
           n n]
      (if (> n 0)
        (recur (* pc value) (dec n))
        (double value)))))

(defn to-proper-format [l]
  (apply hash-map (parse-it l)))
  

#_(def raw-btc-price 
   (concat (map make-compatible (historical-price (days-missing))) (dbg raw-btc-price)))
(def raw-btc-price 
  (concat (map to-proper-format (partition 7 (.split (slurp (clojure.java.io/resource "tmpbtcprice")) "\n"))) raw-btc-price))

(defn last-of [ix n]
  (take n (drop ix raw-btc-price)))


(defn moving-average-of [ix n] 
  (/ (reduce + (map :close (last-of ix n))) n))

(defn moving-averages-of 
  "Get all n moving averages from ix going back in time back-in-days"
  [ix n back-in-days] 
  (map (fn [x] (moving-average-of x n)) (range back-in-days)))

(defn rising? [data]
  (loop [l data
         v 0]
    (if (empty? l)
      true
      (if (> (first l) v)
        (recur (rest l) (first l))
        false))))
      
  

(defn mayer-multiple-of [ix] (/ (-> raw-btc-price (nth ix) :close) (moving-average-of ix 200)))

(def mayer-multiples 
  (map (fn [ix] {:date (-> raw-btc-price (nth ix) :date) :mm (mayer-multiple-of ix)}) (range (- (count raw-btc-price) 200))))

(defn average-multiple []
  (/ (reduce + (map :mm mayer-multiples)) (count mayer-multiples)))


(defn buy-btc? [] (< (mayer-multiple-of 0) (average-multiple)))
  

(defn btc-powerlaw [year] 
  (Math/pow (/ (+ year 1) year) 6))

(defn btc-growth-fn [value start-year]
  (* value (btc-powerlaw start-year)))

(defn btc-growth [value start-year years]
  (reduce 
    (fn [acc v] (btc-growth-fn acc v)) value (range start-year (+ start-year years)))
  )

(defn giovani-price [days] (* (Math/pow 10 -17) (Math/pow days 5.83)))

(defn growth-fn [factor acc v]
  (let [[v y] (first acc)]
    (cons [(* factor v) (inc y)] acc))) 
  

(defn growth [value percent years]
  (let [factor (+ 1 (/ percent 100))]
    (reduce (partial growth-fn factor) [[value 0]] (range years)))) 
  
(defn passive-net-income [fortune percent]
  (* 0.7 (/ (- (growth fortune percent 1) fortune) 12)))


(def btc-usd-price [[0.11 2010][20.96 2011][10.89 2012]
                       [101.95 2013][592 2014][244 2015]
                       [764 2016][2942 2017][6164 2018]
                       [8036 2019][9480 2020][35850 2021]
                       [20473 2022][26505 2023][65000 2024]])
(def btc-usd-year-map (apply merge (map (fn [[v y]] {y v}) btc-usd-price))) 

(def usd-sek [[7.84 2010][6.4 2011][6.99 2012]
              [2.56 2013][6.63 2014][8.29 2015]
              [8.33 2016][8.71 2017][8.71 2018]
              [9.54 2019][9.32 2020][8.26 2021]
              [9.54 2022][10.8 2023][10.52 2024]])

(def usd-sek-year-map (apply merge (map (fn [[v y]] {y v}) usd-sek))) 

(def btc-sek-price-map (apply merge (map (fn [[v y]] {y (* v (get btc-usd-year-map y))}) usd-sek)))

(def kardborrevagen-btc-price
  (let [v (reverse (map (fn [[v y]] [v (+ 2003 y)]) (growth 2.5e6 5.5 21)))]
    (map 
      (fn [[v y]]
        (if-let [x (dbg (get btc-sek-price-map y))]
          [(/ v x) y]
          [nil y])) 
        v)
    ))


(String/format "You should %sbuy btc!" (into-array String [(if (buy-btc?) "" "not ")]))


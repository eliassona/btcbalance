(ns btcbalance.bigmac
  (:require [btcbalance.core :refer [url-get raw-btc-price growth]]
            [bchain.core :refer [satoshi dbg]])
  (:use [clojure.pprint])
  (:import [java.net URL]
           [java.text SimpleDateFormat]))

(def csv-file "https://raw.githubusercontent.com/TheEconomist/big-mac-data/master/output-data/big-mac-raw-index.csv")

(def raw-csv-data (url-get csv-file))

(def line-split-csv-data (.split raw-csv-data "\n"))

(def raw-csv-split-data (map #(.split % ",") line-split-csv-data))

(def header (map keyword (first raw-csv-split-data)))

(def csv-split-data (rest raw-csv-split-data))

(defn csv->map [data] (zipmap header data))

(def big-mac-data (map csv->map  csv-split-data))

(defn big-mac-date->java-date [date] (.parse (SimpleDateFormat. "yyyy-MM-dd") date))

(defn btc-date->java-date [date] (.parse (SimpleDateFormat. "MMM dd, yyyy") date))

(def big-mac-dollar-price (filter #(= (:currency_code %) "USD") big-mac-data))

(def btc-price-data 
  (map 
    (fn [d]
      (let [date (-> d :date btc-date->java-date)]
        (assoc d :date date))) raw-btc-price))
         

(defn btc-price-of [date]
  (first (filter #(= date (:date %)) btc-price-data)))




(defn big-mac-usd->btc [big-mac-usd]
  {:date (:date big-mac-usd),
   :currency_code "SAT",
   :local_price
  (when-let [btc-data (-> big-mac-usd :date big-mac-date->java-date btc-price-of)]
    (long (* (/ (-> big-mac-usd :local_price read-string) (:close btc-data)) 1e8)))})
  

(def gold-price 
  [{:date "2000-04-01" :local_price 303}
   {:date "2001-04-01" :local_price 258.8}
   {:date "2002-04-01" :local_price 303.7}
   {:date "2003-04-01" :local_price 342.85}
   {:date "2004-05-01" :local_price 379.5}
   {:date "2005-06-01" :local_price 439.15}
   {:date "2006-01-01" :local_price 558.7}
   {:date "2006-05-01" :local_price 665.7}
   {:date "2007-01-01" :local_price 646.84}
   {:date "2007-06-01" :local_price 656.15}
   {:date "2008-06-01" :local_price 895}
   {:date "2009-07-01" :local_price 953}
   {:date "2010-01-01" :local_price 1150.7}
   {:date "2010-07-01" :local_price 1196}
   {:date "2011-07-01" :local_price 1589}
   {:date "2012-01-01" :local_price 1662}
   {:date "2012-07-01" :local_price 1618}
   {:date "2013-01-01" :local_price 1660}
   {:date "2013-07-01" :local_price 1312}
   {:date "2014-01-01" :local_price 1264}
   {:date "2014-07-01" :local_price 1171}
   {:date "2015-01-01" :local_price 1226}
   {:date "2015-07-01" :local_price 1171}
   {:date "2016-01-01" :local_price 1097}
   {:date "2016-07-01" :local_price 1355}
   {:date "2017-01-01" :local_price 1235}
   {:date "2017-07-01" :local_price 1247}
   {:date "2018-01-01" :local_price 1327}
   {:date "2018-07-01" :local_price 1258}
   {:date "2019-01-01" :local_price 1316}
   {:date "2019-07-09" :local_price 1426}
   {:date "2020-01-14" :local_price 1524}
   {:date "2020-07-01" :local_price 1575}])

(defn gold-price-of [date]
  (:local_price (first (filter #(= date (:date %)) gold-price))))

(defn big-mac-usd->gold [big-mac-usd]
  (let [d (:date big-mac-usd)]
    (when-let [gp (gold-price-of d)]
      {:date d,
       :currency_code "GOLD",
       :local_price (/ (-> big-mac-usd :local_price read-string) gp)})))

(ns btcbalance.bigmac
  (:require [btcbalance.core :refer [url-get raw-btc-price]]
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
   :currency_code "BTC",
   :local_price
  (when-let [btc-data (-> big-mac-usd :date big-mac-date->java-date btc-price-of)]
    (long (* (/ (-> big-mac-usd :local_price read-string) (:close btc-data)) 1e8)))})
  


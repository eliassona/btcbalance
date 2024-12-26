(ns btcbalance.core
  (:use [clojure.pprint])
  (:require [bchain.core :refer [SEK SEK-last USD USD-last EUR-last satoshi]]
            [clj-http.client :as client]
            [hickory.core :as hickory]
            [hickory.select :as s]
            #_[bchain.shape :as ss]
            #_[bchain.balance :refer [convert balance def-balance total-balance convert-to store! all-balance store-proc]]
            
            )
  (:import [java.io File] 
           [java.net URL]
           [java.text SimpleDateFormat]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(def home-dir (File. (System/getProperty "user.home"), "btcbalance"))

(when (not (.exists home-dir))
  (.mkdirs home-dir))

(defn url-get [url]
  (let [con (.openConnection (URL. url))
        _ (.setRequestMethod con "GET")
        in (.getInputStream con)
        ]
    (try
      (slurp in)
      (finally (.close in)))))


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

(defn decay-fn [factor year decay]
  (dbg (max 1.04 (- factor (* year decay)))))

(defn growth-fn [factor decay withdraw acc v]
  (let [[v y w] (first acc)
        wd-value (/ (* withdraw v) 100)]
    (cons [(double (* (decay-fn factor y decay) (- v wd-value))) (inc y) wd-value] acc))) 
  

(defn growth 
  ([value percent years decay withdraw]
    (let [factor (+ 1 (/ percent 100))]
      (reduce (partial growth-fn factor (/ decay 100) withdraw) [[value 0 0]] (range years))))
  ([value percent years decay]
    (growth value percent years decay 0))
  ([value percent years]
    (growth value percent years 0)))
  
(defn growth->money-fn [[fortune year withdraw]]
  [(money fortune) year (money withdraw)]) 

(defn cagr-of [start end years]
  (let [diff (- end start)
        step (cond (= diff 0) 0
                   (> diff 0) 0.1
                   :else -0.1)]
    (if (= step 0)
      0
      (loop [percent step]
        (let [end-value (-> (growth start percent years) first first)]
          (if (> end-value end)
            percent
            (recur (+ percent 0.1))))))))

(defn passive-net-income [fortune percent]
  (* 0.7 (/ (- (growth fortune percent 1) fortune) 12)))


(def btc-usd-price [[0.11 2010][20.96 2011][10.89 2012]
                       [101.95 2013][592 2014][244 2015]
                       [764 2016][1200 2017][6164 2018]
                       [8036 2019][9480 2020][35850 2021]
                       [20473 2022][26505 2023][65000 2024]])

(def gold-usd [[360 2003][393.9 2004][416.4 2005]
               [642.8 2006][661.5 2007][887.3 2008]
               [978.8 2009][1224.8 2010][1535.7 2011]
               [1560 2012][1389.1 2013][1250 2014]
               [1190.6 2015][1215.7 2016][1268 2017]
               [1298.6 2018][1307 2019][1740.4 2020]
               [1905.1 2021][1831 2022][1963.2 2023]
               [2322.6 2024]
])

(def btc-usd-year-map (apply merge (map (fn [[v y]] {y v}) btc-usd-price))) 

(def usd-sek [[7.36 2003][7.44 2004][7.30 2005]
              [7.33 2006][7.04 2007][6.08 2008]
              [7.81 2009]
              [7.84 2010][6.4 2011][6.99 2012]
              [6.43 2013][6.63 2014][8.29 2015]
              [8.33 2016][8.71 2017][8.71 2018]
              [9.54 2019][9.32 2020][8.26 2021]
              [9.54 2022][10.8 2023][10.52 2024]])

(def chf-sek 
  [[5.78 2003][5.94 2004][6.09 2005]
   [5.95 2006][5.67 2007][5.8 2008]
   [6.89 2009][7.04 2010][7.76 2011]
   [7.35 2012][6.98 2013][7.41 2014]
   [8.91 2015][8.7 2016][8.99 2017]
   [8.81 2018][9.49 2019][9.95 2020]
   [9.33 2021][10.15 2022][11.99 2023]
   [11.76 2024]
   ]
  )

(def usd-sek-year-map (apply merge (map (fn [[v y]] {y v}) usd-sek))) 
(def chf-sek-year-map (apply merge (map (fn [[v y]] {y v}) chf-sek))) 
(def gold-usd-year-map (apply merge (map (fn [[v y]] {y v}) gold-usd))) 

(def btc-sek-price-map (apply merge 
                              (map 
                                (fn [[v y]] 
                                  {y 
                                   (when-let [x (get btc-usd-year-map y)] (* v x))}) 
                                usd-sek)))


(def house-price-sek (reverse (map (fn [[v y]] [v (+ 2003 y)]) (growth 2.5e6 5.5 21))))

(def house-price-btc
    (map 
      (fn [[v y]]
        (if-let [x (get btc-sek-price-map y)]
          [(Math/round (/ v x)) y]
          [nil y])) 
        house-price-sek))

(def house-price-usd
    (map 
      (fn [[v y]]
        (if-let [x (get usd-sek-year-map y)]
          [(Math/round (/ v x)) y]
          [nil y])) 
        house-price-sek))

(def house-price-gold
    (map 
      (fn [[v y]]
        (if-let [x (get gold-usd-year-map y)]
          [(Math/round (float (/ v x))) y]
          [nil y])) 
        house-price-usd))

(def house-price-chf
    (map 
      (fn [[v y]]
        (if-let [x (get chf-sek-year-map y)]
          [(Math/round (/ v x)) y]
          [nil y])) 
        house-price-sek))



(defn get-stock-page [symbol]
  (let [url (str "https://finance.yahoo.com/quote/" symbol)
        response (client/get url {:headers {"User-Agent" "Mozilla/5.0"}})] ;; Add User-Agent header to mimic a browser
    (:body response)))

(defn parse-html [html]
  (-> html
      hickory/parse
      hickory/as-hickory))

(defprotocol Ticker 
  (ticker-of [obj sym]))

(defn entry-of [sym e]
  (let [v (ticker-of (val e) sym)]
    (when v
      e)))
  

(extend-protocol Ticker
  clojure.lang.Keyword
  (ticker-of [s kw] kw)
  String
  (ticker-of [s sym] (if (>= (.indexOf s sym) 0) s nil))
  java.util.Map
  (ticker-of [m sym] (filter identity (map (partial entry-of sym) m)))
  java.util.List
  (ticker-of [l sym] (filter identity (map #(ticker-of % sym) l)))
  )


#_(String/format "You should %sbuy btc!" (into-array String [(if (buy-btc?) "" "not ")]))
(defn sek-usd-rate [] (/ (SEK-last) (USD-last)))
(defn sek-eur-rate [] (/ (SEK-last) (EUR-last)))
(defn money [n]
  (let [n (long n)
        formatted (String/format (java.util.Locale/US) "%,d" (object-array [n]))]
    (clojure.string/replace formatted #"," "'")))
(def total-nr-of-btc 21e6)
(def world-population 5.5e9)
(def sweden-population 10e6)
(def btc-per-capita (/ total-nr-of-btc world-population))
(def world-median-welth-usd 9300)
(def world-average-welth-usd 84718)
(def total-welth-usd (* world-average-welth-usd world-population))
(defn hyper-bitcoinized-usd-value-of [btc average-welth]
  (/ (* average-welth (/ btc btc-per-capita)) 2))
  
  




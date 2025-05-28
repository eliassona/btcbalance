(ns btcbalance.core
  (:use [clojure.pprint])
  (:require [bchain.core :refer [SEK SEK-last USD USD-last EUR-last satoshi]]
            [clj-http.client :as client]
            [hickory.core :as hickory]
            [hickory.select :as s]
            [clojure.data :as data]
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

(def date-template (SimpleDateFormat. "MM/dd/yyyy"))

(defn date-str->unix-time [d] (.parse date-template d))

(defn split-it [v]
  (let [ix0 (inc (.indexOf v "\""))
        ix1 (.indexOf v "\"" ix0)
        ix2 (inc (.indexOf v "\"" (inc ix1)))
        ix3 (.indexOf v "\"" ix2)
        ]
  [(.substring v ix0 ix1)
   (parse-price (.substring v ix2 ix3))]
  ))

(def bhd-file (File. "/Users/anderseliasson/src/btcbalance/resources/Bitcoin_Historical_Data.csv"))

(def raw-btc-price
    (map split-it 
         (.split (slurp bhd-file) "\n")))

(defn curr-ticket []
  (format "\"%s\", \"%s\"" (.format date-template (java.util.Date.)) (USD-last))
  )
(defn save! [] 
  (let [text (slurp bhd-file)]
    (spit bhd-file (format "%s\n%s" (curr-ticket) text))))


(defn- latest-time []
  (.getTime (date-str->unix-time (first (first raw-btc-price)))))

(defn- current-time [] (System/currentTimeMillis))

(def day-in-millis (* 24 3600 1000))

(defn- days-missing [] (inc (int (/ (- (current-time) (latest-time)) day-in-millis))))

(defn last-of [ix n]
  (take n (drop ix raw-btc-price)))


(defn moving-average-of [ix n] 
  (/ (reduce + (map second (last-of ix n))) n))

(defn moving-averages-of 
  "Get all n moving averages from ix going back in time back-in-days"
  [ix n back-in-days] 
  (map (fn [x] (moving-average-of (+ ix x) n)) (range back-in-days)))




(defn rising? [data]
  (loop [l data
         v 0]
    (if (empty? l)
      true
      (if (> (first l) v)
        (recur (rest l) (first l))
        false))))
      
(defn last-price-of [ix]
  (if (neg? ix)
    (USD-last)
    (-> raw-btc-price (nth ix) second)))

(defn mayer-multiple-of [ix]
  (let [ma (moving-average-of ix 200)]
  (/ (last-price-of ix) ma)))

(def mayer-multiples 
  (map (fn [ix] {:date (-> raw-btc-price (nth ix) first) :mm (mayer-multiple-of ix)}) (range (- (count raw-btc-price) 200))))

(defn average-multiple []
  (/ (reduce + (map :mm mayer-multiples)) (count mayer-multiples)))

(defn btc-price-at-ix [ix]
  (-> raw-btc-price (nth ix) second))


(defn date->value [date-str]
  (loop [rbp raw-btc-price]
    (when (not (empty? rbp))
      (let [[d v] (first rbp)]
        (if (= d date-str)
          v
          (recur (rest rbp)))))))

(defn current-year []
  (+ 1900 (.getYear (java.util.Date.))))

(defn btc-price-on-day 
  ([month day year]
    (loop [year year
           result []]
      (let [d (format "%02d/%02d/%s" month day year)
            v (date->value d)]
        (if v
          (recur (dec year) (conj result [d v]))
          result))))
  ([]
    (let [d (java.util.Date.)]
      (btc-price-on-day (inc (.getMonth d)) (.getDate d) (current-year)))))

(defn first-day-value []
  (format "01/01/%s" (current-year)))
  
(defn value-delta->procent [v1 v2]
  (double (* 100 (- (/ v1  v2) 1))))

(defn return-of ([start end]
  (value-delta->procent (btc-price-at-ix start) (btc-price-at-ix end)))
  ([period]
    (condp =
      :ytd (value-delta->procent 
             (btc-price-at-ix 0) 
             (date->value (first-day-value))))))



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
  (max 1.04 (- factor (* year decay))))

 
(defn growth-fn [factor acc year]
  (let [last-value (-> acc first first)]
    (cons [(double (+ last-value (* last-value factor))) year] acc)))  

(defn growth 
  ([value percent years]
    (let [factor (/ percent 100)]
      (reduce (partial growth-fn factor) [[value 0]] (range 1 (inc years))))))

(defn worst-month [value years]
  (let [ma (map (fn [[v1 v2]] (/ v1 v2)) (partition 2 (moving-averages-of 0 (* 200 7) (* years 365))))
        m (growth value (* (- (apply min ma) 1) 100) 30)]
    (- (-> m first first) (-> m last first))))

(defn growth-goal [value percent years]
  (-> (growth value percent years) first first))

#_(defn growth->money-fn [[fortune year withdraw]]
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
  

(defn sats-price [price-sek]
  (long (/ price-sek (SEK-last) satoshi)))
(defn egg-30 [] (sats-price 125))
(defn gas [] (sats-price 17))

(defn update-dest-unit [ratio [k v]]
  (dbg [k (/ v ratio)]))

(defn update-dest-units [ratio m]
  (dbg (into {} (map (partial update-dest-unit ratio) (dbg m)))))

(defn update-stock [ticker ratio m]
  (update-in (dbg m) [1 ticker] (partial update-dest-units ratio)))

(defn stock-split [rates ticker ratio index]
  (concat 
    (take index rates) 
    (map (partial update-stock ticker ratio)  (drop index rates))))









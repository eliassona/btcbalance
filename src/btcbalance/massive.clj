(ns btcbalance.massive
  (:require [clj-http.client :as http]
    [cheshire.core :as json]
    [clojure.string :as str]))


(def BASE-URL "https://api.massive.com/v3/reference/dividends")

(defn fetch-dividends
  "Fetch recent dividends for a ticker (default: latest 10)"
  [api-key ticker]
   (let [url     BASE-URL
         params  {:ticker ticker
                  :limit  1
                  :order  "desc"
                  :sort   "ex_dividend_date"
                  :apikey api-key}
         resp    (http/get url {:query-params params
                                :accept :json})
         body    (:body resp)]
     (json/parse-string body true))) ; true → keyword keys
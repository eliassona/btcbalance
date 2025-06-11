(ns btcbalance.finnhub
  (:require [clj-http.client :as http]
            [cheshire.core :as json]))

(defn fetch-stock-quote
  "Fetches the latest stock quote from Finnhub API for a given symbol."
  [api-key symbol]
  (let [url "https://finnhub.io/api/v1/quote"
        params {:query-params
                {"symbol" symbol
                 "token" api-key}
                :socket-timeout 1000  
                :connection-timeout 1000}]
    (try
      (let [response (http/get url params)
            body (json/parse-string (:body response) true)]
        (if (:c body)
          {:symbol symbol
           :current (:c body)  ;; Current price
           :open (:o body)     ;; Open price
           :high (:h body)     ;; High price
           :low (:l body)      ;; Low price
           :previous-close (:pc body) ;; Previous close price
           :timestamp (:t body)} ;; Unix timestamp
          (throw (ex-info "No quote data found" {:response body}))))
      (catch Exception e
        {:error (str "Failed to fetch quote: " (.getMessage e))}))))
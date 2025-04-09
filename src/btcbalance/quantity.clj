(ns btcbalance.quantity
  (:require [bchain.core :refer [SEK SEK-last USD USD-last EUR-last satoshi]])
  )
(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn assoc-inv [m]
  (reduce 
    (fn [acc v]
      (let [[from to] (key v)
            the-fn (val v)]
        (assoc acc [to from] (fn [] (/ 1 (the-fn))))))
    m m)
  )
  
  
(def conversion-map 
  (assoc-inv {[:usd :btc] USD-last
              [:sek :btc] SEK-last}))

(defrecord Quantity [value unit])

(defn convert 
  ([q to-unit visited-units]
    (let [cm (get conversion-map [to-unit (:unit q)])]
  (* (:value q) (cm))))
  ([q to-unit]
   (convert q to-unit #{}))) 

(defn do-arith [op q1 q2]
  (let [u1 (:unit q1)
        u2 (:unit q2)]
    (Quantity. (op (:value q1) (if (= u1 u2) (:value q2) (convert q2 u1))) u1)))
  
  

(defn add [q1 q2]
  (do-arith + q1 q2))
  



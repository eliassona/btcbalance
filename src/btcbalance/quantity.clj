(ns btcbalance.quantity
  (:require [bchain.core :refer [SEK SEK-last USD USD-last EUR-last satoshi]])
  )
(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))



(defn inv-fn-of [the-fn]
  (fn [] (/ 1 (the-fn))))


(defn inv-of [m]
  (let [x (partition 
            3
            (flatten 
              (map 
                (fn [k] 
                  (map 
                    (fn [e] [(key e) k (inv-fn-of (val e))]) 
                    (get m k))) 
                (keys m))))]
    (reduce 
      (fn [acc [from-unit to-unit the-fn]]
        (update acc from-unit assoc to-unit the-fn)
        ) 
      m 
      x)
    ))



(def conversion-map 
  (inv-of 
    {:btc {:usd USD-last
           :sek SEK-last}
     }))

(def unit-set 
  (into #{} (flatten (map (fn [e] [(key e) (-> e val keys)]) conversion-map))))


(defrecord Quantity [value unit])

(defn to-fn-of [from-unit to-unit]
  (-> conversion-map (get from-unit) (get to-unit)))

(defn path-of 
  ([from-unit to-unit visited-units]
    
    )
  ([from-unit to-unit]
    (when-let [p (path-of from-unit to-unit (disj unit-set from-unit))]
      (cons from-unit p) 
    )))

(defn convert 
  ([q to-unit visited-units]
    (let [from-unit (:unit q)
          to-fn (to-fn-of from-unit to-unit)]
          (if (nil? to-fn)
            :failure
            (* (:value q) (to-fn)))))
  ([q to-unit]
   (convert q to-unit #{}))) 

(defn do-arith [op q1 q2]
  (let [u1 (:unit q1)
        u2 (:unit q2)]
    (Quantity. (op (:value q1) (if (= u1 u2) (:value q2) (convert q2 u1))) u1)))
  
  

(defn add [q1 q2]
  (do-arith + q1 q2))
  



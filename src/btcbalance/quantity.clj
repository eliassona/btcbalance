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

(defn to-fn-of [c-map from-unit to-unit]
  (-> c-map (get from-unit) (get to-unit)))

(defn unit-path-of 
  ([from-unit to-unit cm]
    (let [units (into #{} (-> cm from-unit keys))]
      (if (contains? units to-unit)
        [from-unit to-unit]
        (let [cm (dissoc cm from-unit)]
          (loop [us units]
            (if (empty? us)
              nil
              (if-let [up (unit-path-of (first us) to-unit cm)]
                (cons from-unit up)
                (recur (rest units)))))))))
  ([from-unit to-unit]
    (unit-path-of from-unit to-unit conversion-map)))

(defn path->fns [units]
  (if (> (count units) 1)
     (cons 
       (to-fn-of conversion-map (first units) (second units)) 
       (path->fns (rest units)))
     []))

(defn indirect-fn-of [fns]
  (fn [] (reduce (fn [acc v] (* acc (v))) 1 fns)))

(defn to-indirect-fn [q to-unit]
  (let [from-unit (:unit q)]
    (if-let [units (unit-path-of from-unit to-unit)]
      (let [fns (path->fns units)]
        (indirect-fn-of fns))
      (throw 
        (IllegalArgumentException. 
          (format "Unit %s cannot be converted to %s" from-unit to-unit))))))       

(defn convert 
  [q to-unit]
    (let [from-unit (:unit q)
          to-fn (to-fn-of conversion-map from-unit to-unit)
          to-fn (if to-fn to-fn (to-indirect-fn q to-unit))]
            (* (:value q) (to-fn))))

(defn do-arith [op q1 q2]
  (let [u1 (:unit q1)
        u2 (:unit q2)]
    (Quantity. (op (:value q1) (if (= u1 u2) (:value q2) (convert q2 u1))) u1)))
  
  

(defn add [q1 q2] (do-arith + q1 q2))
(defn sub [q1 q2] (do-arith - q1 q2))
(defn mul [q1 q2] (do-arith * q1 q2))
(defn div [q1 q2] (do-arith / q1 q2))
  



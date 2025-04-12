(ns btcbalance.quantity
  (:require [bchain.core :refer [SEK SEK-last USD USD-last EUR-last JPY-last satoshi]])  
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

(def the-conversion-map (atom {}))

(defn set-conversion-map! [m]
  (reset! the-conversion-map (inv-of m)))

(defn unit-set []
  (into #{} (flatten (map (fn [e] [(key e) (-> e val keys)]) @the-conversion-map))))


(defrecord Quantity [value unit])

(defn to-fn-of [from-unit to-unit]
  (-> @the-conversion-map (get from-unit) (get to-unit)))

(defn unit-path-of 
  ([from-unit to-unit visited-units]
    (let [units (apply disj (into #{} (-> @the-conversion-map from-unit keys)) visited-units)]
      (if (empty? units)
        nil
      (if (contains? units to-unit)
        [from-unit to-unit]
        (loop [us units]
          (if (empty? us)
            nil
            (if-let [up (unit-path-of (first us) to-unit (conj visited-units (first us)))]
              (cons from-unit up)
              (recur (rest us)))))))))
  ([from-unit to-unit] (unit-path-of from-unit to-unit #{from-unit})))

(defn path->fns [units]
  (if (> (count units) 1)
     (cons 
       (to-fn-of (first units) (second units)) 
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
          to-fn (to-fn-of from-unit to-unit)
          to-fn (if to-fn to-fn (to-indirect-fn q to-unit))]
            (Quantity. (* (:value q) (to-fn)) to-unit)))

(defn do-add-sub 
  ([op q1] q1)
  ([op q1 q2]
  (let [u1 (:unit q1)
        u2 (:unit q2)]
    (Quantity. (op 
                 (:value q1) 
                 (:value (if (= u1 u2) q2 (convert q2 u1)))) 
               u1)))
  ([op q1 q2 & args]
    (reduce (fn [acc v] (do-add-sub op acc v)) (do-add-sub op q1 q2) args))
  )

(defn do-mul-div [op args]
  (let [q (first args)]
    (->Quantity (apply * (cons (:value q) (rest args))) (:unit q))))
  
(defn apply-op [op args] (apply (partial do-add-sub op) args))
(defn add [& args] (apply-op + args))
(defn sub [& args] (apply-op - args))
(defn mul [& args] (do-mul-div * args))
(defn div [& args] (do-mul-div / args))
(defn rate [from-unit to-unit]
  (:value (convert (->Quantity 1 from-unit) to-unit)))
  



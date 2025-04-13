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

(defn unit-set [cm]
  (into #{} (flatten (map (fn [e] [(key e) (-> e val keys)]) cm))))


(defrecord Quantity [value unit])
(defn quantity [value unit]
  (->Quantity value unit))

(defn to-fn-of [cm from-unit to-unit]
  (-> cm (get from-unit) (get to-unit)))

(defn unit-path-of 
  ([cm from-unit to-unit visited-units]
    (let [units (apply disj (into #{} (-> cm from-unit keys)) visited-units)]
      (if (empty? units)
        nil
      (if (contains? units to-unit)
        [from-unit to-unit]
        (loop [us units]
          (if (empty? us)
            nil
            (if-let [up (unit-path-of cm (first us) to-unit (conj visited-units (first us)))]
              (cons from-unit up)
              (recur (rest us)))))))))
  ([cm from-unit to-unit] (unit-path-of cm from-unit to-unit #{from-unit})))

(defn path->fns [cm units]
  (if (> (count units) 1)
     (cons 
       (to-fn-of cm (first units) (second units)) 
       (path->fns cm (rest units)))
     []))

(defn indirect-fn-of [fns]
  (fn [] (reduce (fn [acc v] (* acc (v))) 1 fns)))

(defn to-indirect-fn [cm q to-unit]
  (let [from-unit (:unit q)]
    (if-let [units (unit-path-of cm from-unit to-unit)]
      (let [fns (path->fns cm units)]
        (indirect-fn-of fns))
      (throw 
        (IllegalArgumentException. 
          (format "Unit %s cannot be converted to %s" from-unit to-unit))))))       

(defn convert 
  [cm q to-unit]
    (let [from-unit (:unit q)]
      (if (= from-unit to-unit)
        q
        (let [to-fn (to-fn-of cm from-unit to-unit)
              to-fn (if to-fn to-fn (to-indirect-fn cm q to-unit))]
          (Quantity. (* (:value q) (to-fn)) to-unit)))))

(defn do-add-sub 
  ([cm op q1] q1)
  ([cm op q1 q2]
  (let [u1 (:unit q1)
        u2 (:unit q2)]
    (Quantity. (op 
                 (:value q1) 
                 (:value (if (= u1 u2) q2 (convert cm q2 u1)))) 
               u1)))
  ([cm op q1 q2 & args]
    (reduce (fn [acc v] (do-add-sub cm op acc v)) (do-add-sub cm op q1 q2) args))
  )

(defn do-mul-div [op args]
  (let [q (first args)]
    (->Quantity (apply op (cons (:value q) (rest args))) (:unit q))))
  
(defn apply-op [cm op args] (apply (partial do-add-sub cm op) args))

(defn add [cm & args] (apply-op cm + args))

(defn sub [cm & args] (apply-op cm - args))

(defn mul [& args] (do-mul-div * args))

(defn div [& args] (do-mul-div / args))

(defn rate [cm from-unit to-unit]
  (:value (convert cm (quantity 1 from-unit) to-unit)))
  
(defn do-comparator [cm op q1 q2]
  (op (:value q1) (:value (convert cm q2 (:unit q1)))))

(defn gt [cm q1 q2] (do-comparator cm > q1 q2))
(defn gte [cm q1 q2] (do-comparator cm <= q1 q2))
(defn lt [cm q1 q2] (do-comparator cm < q1 q2))
(defn lte [cm q1 q2] (do-comparator cm <= q1 q2))
(defn eq [cm q1 q2] (do-comparator cm = q1 q2))
(defn not-eq [cm q1 q2] (do-comparator cm not= q1 q2))


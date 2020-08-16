(ns clojure-practice.transducer
  (:require
   [clojure.core.async :refer [go-loop <! >! close!]]))

(defn mymap [f coll]
  (when (not= '() coll)
    (conj
     (mymap f (rest coll))
     (f (first coll)))))

;; (defn myfilter [pred coll]
;;   (when (not= '() coll)
;;     (if (pred (first coll))
;;       (conj (myfilter pred (rest coll)) (first coll))
;;       (myfilter pred (rest coll)))))

;; (defn map [f in out]
;;   (go-loop []
;;     (let [val (<! in)]
;;       (if (nil? val)
;;         (close! out)
;;         (do (doseq [v (f val)]
;;               (>! out v))
;;             (when-not (impl/clsed? out)
;;               (recur)))))))

;; (defn reduce [f result coll]
;;   (if (not= '() coll)
;;     (reduce f (f result (first coll)) (rest coll))
;;     result))

;; (defn mymap [f result coll]
;;   (if (not= '() coll)
;;     (mymap f (f result (first coll)) (rest coll))
;;     result))

;; (mymap (fn [result el] (conj result (inc el))) [] (range 10))

;; (defn reduce [f result coll]
;;   (if (not= '() coll)
;;     (reduce f (f result (first coll)) (rest coll))
;;     result))

;; (defn mapping [result el]
;;   (conj result (inc el)))

;; (reduce mapping [] (range 10))

;; (defn filtering [result el]
;;   (if (odd? el)
;;     (conj result el)
;;     result))

;; (reduce filtering [] (range 20))

;; (defn mapping [rf]
;;   (fn [result el]
;;     (rf result (inc el))))

;; (defn filtering [rf]
;;   (fn [result el]
;;     (if (odd? el)
;;       (rf result el)
;;       result)))


;; (defn mapping [f]
;;   (fn [rf]
;;     (fn [result el]
;;       (rf result (f el)))))

;; (reduce ((mapping inc) conj) [] (range 10))
;; (transduce (mapping inc) conj (range 10))
;; (refer-clojure)
;; (map inc)
;; (defn mapreducefun [result el]
;;   (if (nil? el)
;;     result
;;     (conj result el)))

;; (let [transfun (map inc)
;;       mapreducefun (fn [result el]
;;                      (if (nil? el)
;;                        result
;;                        (conj result (inc el))))
;;       transmapreducefun (transfun mapreducefun)]
;;   (transmapreducefun [] 20))

;; (let [transfun (map inc)]
;;   (transfun [1 2 3]))

;; (reduce + (filter odd? (map #(+ 2 %) (range 10))))

;; (apply str [1 2 3 "b" [1 2 4]])
;; (apply str "d" [1 2 3 "a"] "c")

;; (let [xform (comp
;;              (partial filter odd?)
;;              (partial map #(+ 2 %))
;;              )]
;;   (reduce + (xform (range 10))))

;; (let [xform (fn [xs] (->> xs
;;                           (map #(+ 2 %))
;;                           (filter odd?)))]
;;   (reduce + (xform (range 20))))

;; (let [xform (comp 
;;              (filter odd?)
;;              (map inc))]
;;   (into [] xform (range 100)))
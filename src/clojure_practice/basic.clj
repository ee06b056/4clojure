(ns clojure-practice.basic)

(defn relay [x i]
  (prn i)
  (when (:next x)
    (send (:next x) relay i))
  (when (and (zero? i) (:report-queue x))
    (.put (:report-queue x) i))
  (prn x)
  x)

(defn run [m n]
  (let [q (new java.util.concurrent.SynchronousQueue)
        hd (reduce (fn [next _] (agent {:next next}))
                   (agent {:report-queue q}) (range (dec m)))]
    (doseq [i (reverse (range n))]
      (send hd relay i))
    (.take q)))

(time (run 10 10))

; (def a (reduce (fn [next _] (agent {:next next}))
;         (agent {:report-queue []})
;         (range (dec 100))))

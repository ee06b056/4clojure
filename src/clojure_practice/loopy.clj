(ns clojure-practive.loopy)

(defn prime-factors [n]
  (loop [n n
         d 2
         factors []]
    (if (> n 1)
      (if (zero? (mod n d))
        (recur (/ n d) d (conj factors d))
        (recur n (inc d) factors))
      factors)))
(ns clojure-practice.core
  (:gen-class))
;; 07/17/2019
;; #62: Re-implement Iterate
;;
;; Given a side-effect free function f and an initial value x write a function
;; which returns an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
;;
;; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
;; (= (take 100 (__ inc 0)) (take 100 (range)))
;; (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))
(fn foo [f x]
  (cons x (lazy-seq (foo f (f x)))))

;; 07/18/2019
;; #166 Comparisons
;;
;; For any orderable data type it's possible to derive all of the basic comparison
;; operations (<, ≤, =, ≠, ≥, and >) from a single operation (any operator but = or ≠ will work).
;; Write a function that takes three arguments, a less than operator for the data and
;; two items to compare. The function should return a keyword describing the relationship
;; between the two items. The keywords for the relationship between x and y are as follows:
;;
;; x = y → :eq
;; x > y → :gt
;; x < y → :lt
;;
;; (= :gt (__ < 5 1))
;; (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
;; (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
;; (= :gt (__ > 0 2))
;;
;; solution 1:
(fn [f v1 v2]
  (let [compare1 (f v1 v2)
        compare2 (f v2 v1)
        m {false {false :eq, true :gt}, true {false :lt}}]
    (get-in m [compare1 compare2])))
;; solution 2:
(fn [f a b]
  (cond
    (f a b) :lt
    (f b a) :gt
    :else :eq))

; #90 Cartesian Product
;
; Write a function which calculates the Cartesian product of two sets.
;
; (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
;    #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
;      ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
;      ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
;
; (= (__ #{1 2 3} #{4 5})
;    #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
;
; (= 300 (count (__ (into #{} (range 10))
;                   (into #{} (range 30)))))
; solution 1:
(fn [set1 set2]
  (into #{}
        (mapcat
         (fn [n1]
           (map #(vector n1 %) set2)) set1)))
; solution 2:
(fn [set1 set2]
  (into #{} (for [x set1
        y set2]
    [x y])))
; solution 3:
#(into #{} (for [x %1, y %2] [x y]))

; #99: Product Digits
;
; Write a function which multiplies two numbers and returns the result as a sequence of its digits.
;
; (= (__ 1 1) [1])
;
; (= (__ 99 9) [8 9 1])
;
; (= (__ 999 99) [9 8 9 0 1])
; solution 1:
(fn [a b]
  (map #(Integer/parseInt (str %)) (str (* a b))))

; #63: Group a Sequence
;
; Given a function f and a sequence s, write a function which
; returns a map. The keys should be the values of f applied to
; each item in s. The value at each key should be a vector of
; corresponding items in the order they appear in s.
;
; (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
;
; (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
;    {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
;
; (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
;    {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})
; solution 1:
(fn [f sqn]
  (reduce #(assoc %1 (f %2) (conj (get %1 (f %2) []) %2)) {} sqn))
; solution 2:
#(apply merge-with concat (for [v %2] {(%1 v) (vector v)}))
; solution 3:
#(apply merge-with into (for [v %2] {(%1 v) [v]}))

; #88: Symmetric Difference
;
; Write a function which returns the symmetric difference of two sets.
; The symmetric difference is the set of items belonging to one but not both of the two sets.
;
; (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
;
; (= (__ #{:a :b :c} #{}) #{:a :b :c})
;
; (= (__ #{} #{4 5 6}) #{4 5 6})
;
; (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
; solution 1:
(fn [s1 s2]
  (let [s (clojure.set/union s1 s2)]
    (set (filter #(not= (s1 %) (s2 %)) s))))
; solution 2:
#(clojure.set/union (clojure.set/difference %1 %2) (clojure.set/difference %2 %1))

; #143: dot product
;
; Create a function that computes the dot product of two sequences.
; You may assume that the vectors will have the same length.
;
; (= 0 (__ [0 1 0] [1 0 0]))
;
; (= 3 (__ [1 1 1] [1 1 1]))
;
; (= 32 (__ [1 2 3] [4 5 6]))
;
; (= 256 (__ [2 5 6] [100 10 1]))
; solution 1:
(fn [a b]
  (apply + (map * a b)))
#(apply + (map * %1 %2))
; solution 2:
(reduce + (map * %1 %2))

; #122: Read a binary number
;
; Convert a binary number, provided in the form of a string, to its numerical value.
;
; (= 0     (__ "0"))
;
; (= 7     (__ "111"))
;
; (= 8     (__ "1000"))
;
; (= 9     (__ "1001"))
;
; (= 255   (__ "11111111"))
;
; (= 1365  (__ "10101010101"))
;
; (= 65535 (__ "1111111111111111"))
; solution 1:
(fn [bst]
  (reduce (fn [carry n]
            (if (= n \1)
              (inc (* carry 2))
              (* carry 2))) 0 bst))
; solution 2:
#(Integer/parseInt % 2)

;; #126: Through the Looking Class
;;
;; Enter a value which satisfies the following:
;;
;; (let [x __]
;;   (and (= (class x) x) x))
Class

;; 07/19/2019
;; #135: Infix Calculator
;;
;; Your friend Joe is always whining about Lisps using the prefix notation for math.
;; Show him how you could easily write a function that does math using the infix notation.
;; Is your favorite language that flexible, Joe? Write a function that accepts a variable
;; length mathematical expression consisting of numbers and the operations +, -, *, and /.
;; Assume a simple calculator that does not do precedence and instead just calculates left to right.
;;
;; (= 7  (__ 2 + 5))
;;
;; (= 42 (__ 38 + 48 - 2 / 2))
;;
;; (= 8  (__ 10 / 2 - 1 * 2))
;;
;; (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))
;; solution 1:
(fn [init & args]
  (reduce (fn [init-num [sym num]] (sym init-num num)) init (partition 2 args)))
;; solution 2:
(fn calc [& exp]
  (reduce #(if (fn? %1) (%1 %2) (partial %2 %1)) exp))

;; #157: Indexing Sequences
;;
;; Transform a sequence into a sequence of pairs containing the original elements along with their index.
;;
;; (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
;;
;; (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
;;
;; (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])
;; solution 1:
(fn [sqn]
  (map list sqn (iterate inc 0)))
;; solution 2:
#(map % (range))

;; #97: Pascal's Triangle
;;
;; Pascal's triangle is a triangle of numbers computed using the following rules:
;; - The first row is 1.
;; - Each successive row is computed by adding together adjacent numbers in the row above,
;; and adding a 1 to the beginning and end of the row.
;;
;; Write a function which returns the nth row of Pascal's Triangle.
;;
;; (= (__ 1) [1])
;;
;; (= (map __ (range 1 6))
;;    [     [1]
;;     [1 1]
;;     [1 2 1]
;;     [1 3 3 1]
;;     [1 4 6 4 1]])
;;
;; (= (__ 11)
;;    [1 10 45 120 210 252 210 120 45 10 1])
;; solution 1:
((fn foo [n]
  (if (= n 1)
    [1]
    (concat [1] (map + (foo (dec n)) (rest (foo (dec n)))) [1]))) 2)
;; solution 2:
((fn foo [n]
   (if (= n 1)
     [1]
     (let [pre (foo (dec n))]
       (concat [1] (map + pre (rest pre)) [1]))))

 ;; #118: Re-implement Map
 ;;
 ;; Map is one of the core elements of a functional programming language.
 ;; Given a function f and an input sequence s, return a lazy sequence of (f x) for each element x in s.
 ;;
 ;;
 ;; (= [3 4 5 6 7]
 ;;    (__ inc [2 3 4 5 6]))
 ;;
 ;; (= (repeat 10 nil)
 ;;    (__ (fn [_] nil) (range 10)))
 ;;
 ;; (= [1000000 1000001]
 ;;    (->> (__ inc (range))
 ;;         (drop (dec 1000000))
 ;;         (take 2)))
 ;; solution 1:
(fn foo [f sqn]
   (if (empty? sqn)
     '()
     (cons (f (first sqn)) (lazy-seq (foo f (rest sqn))))))
 ;; solution 2:
 (fn my-map [f sqn]
   (lazy-seq (when-let [s (seq sqn)]
               (cons (f (first s)) (my-map f (rest s))))))

;; #95: To Tree, or not to Tree
;;
;; Write a predicate which checks whether or not a given sequence represents
;; a binary tree. Each node in the tree must have a value, a left child, and a right child.
;
;;  true)
;; (= (__ '(:a (:b nil nil) nil))
;;    true)
;
;; (= (__ '(:a (:b nil nil)))
;;    false)
;
;; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
;;    true)
;
;; (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
;;    false)
;
;; (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
;;    true)
;
;; (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
;;    false)
;
;; (= (__ '(:a nil ()))
;;    false)
;; (= (__ '(:a nil ()))
;;    false)
;; solution 1:
 (fn foo [treenode]
      (if (nil? treenode)
      true
      (and (sequential? treenode) (= (count treenode) 3) (foo (second treenode)) (foo (nth treenode 2)))))



(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

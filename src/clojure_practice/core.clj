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
(fn foo [n]
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

;; 07/23/2019
;; #120: Sum of square of digits
;;
;; Write a function which takes a collection of integers as an argument.
;; Return the count of how many elements are smaller than the sum of their squared component digits.
;; For example: 10 is larger than 1 squared plus 0 squared;
;; whereas 15 is smaller than 1 squared plus 5 squared.
;;
;; (= 8 (__ (range 10)))
;;
;; (= 19 (__ (range 30)))
;;
;; (= 50 (__ (range 100)))
;;
;; (= 50 (__ (range 1000)))
;; solution 1:
(fn [sqn]
  (let [parse-digits (fn [num]
                       (map #(- (int %) 48) (str num)))
        squared-sum (fn [digits]
                      (reduce + (map #(* % %) digits)))]
    (count (filter #(< % (squared-sum (parse-digits %))) sqn))))

;; #128: Recognize Playing Cards
;;
;; A standard American deck of playing cards has four suits - spades, hearts, diamonds, and clubs
;; - and thirteen cards in each suit. Two is the lowest rank, followed by other integers up to ten;
;; then the jack, queen, king, and ace.
;; It's convenient for humans to represent these cards as suit/rank pairs,
;; such as H5 or DQ: the heart five and diamond queen respectively.
;; But these forms are not convenient for programmers, so to write a card game
;; you need some way to parse an input string into meaningful components.
;; For purposes of determining rank, we will define the cards to be valued from 0 (the two) to 12 (the ace)
;; Write a function which converts (for example) the string "SJ" into a map of
;; {:suit :spade, :rank 9}. A ten will always be represented with the single character "T", rather than the two characters "10".
;;
;; (= {:suit :diamond :rank 10} (__ "DQ"))
;;
;; (= {:suit :heart :rank 3} (__ "H5"))
;;
;; (= {:suit :club :rank 12} (__ "CA"))
;;
;; (= (range 13) (map (comp :rank __ str)
;;                    '[S2 S3 S4 S5 S6 S7
;;                      S8 S9 ST SJ SQ SK SA]))
;; solution 1:
(fn [card]
  (let [suits {\D :diamond, \H :heart, \S :spades, \C :club}
        ranks "23456789TJQKA"]
    (assoc {} :suit (get suits (first card)) :rank (.indexOf ranks (int (second card))))))

;; #173: Intro to Destructuring 2
;;
;; Sequential destructuring allows you to bind symbols to parts of sequential things
;; (vectors, lists, seqs, etc.): (let [bindings* ] exprs*) Complete the bindings so all let-parts evaluate to 3.
;;
;; (= 3
;;    (let [[__] [+ (range 3)]] (apply __))
;;    (let [[[__] b] [[+ 1] 2]] (__ b))
;;    (let [[__] [inc 2]] (__)))
;; solution 1:
f vs

;;#100: Least Common Multiple
;;
;; Write a function which calculates the least common multiple.
;; Your function should accept a variable number of positive integers or ratios.
;;
;; (== (__ 2 3) 6)
;;
;; (== (__ 5 3 7) 105)
;;
;; (== (__ 1/3 2/5) 2)
;;
;; (== (__ 3/4 1/6) 3/2)
;;
;; (== (__ 7 5/7 2 3/5) 210)
;; solution 1:
(fn [& nums]
   (let [max-common-div (fn [a b]
                          (if (= 0 b)
                            a
                            (recur b (mod a b))))
         lest-common-mul (fn [a b]
                           (/ (* a b) (max-common-div a b)))]
     (reduce lest-common-mul nums)))

;; #96: Beauty is Symmetry
;;
;; Let us define a binary tree as "symmetric" if the left half of the tree is the mirror image of the right half of the tree.
;; Write a predicate to determine whether or not a given binary tree is symmetric.
;; (see To Tree, or not to Tree for a reminder on the tree representation we're using).
;;
;; (= (__ '(:a (:b nil nil) (:b nil nil))) true)
;;
;; (= (__ '(:a (:b nil nil) nil)) false)
;;
;; (= (__ '(:a (:b nil nil) (:c nil nil))) false)
;;
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;         [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
;;    true)
;;
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;         [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
;;    false)
;;
;; (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
;;         [2 [3 nil [4 [6 nil nil] nil]] nil]])
;;    false)
;; solution 1:
(fn [tree]
  (let [is-treenode (fn [])]))
;; solution 2:
#(= ((fn mirror [[n l r :as all]] (when all [n (mirror r) (mirror l)])) %) %)

;; 08/01/2019
;; #147: Pascal's Trapezoid
;;
;; Write a function that, for any given input vector of numbers, returns an infinite lazy sequence of vectors,
;; where each next one is constructed from the previous following the rules used in Pascal's Triangle.
;; For example, for [3 1 2], the next row is [3 4 3 2].
;;
;; Beware of arithmetic overflow! In clojure (since version 1.3 in 2011),
;; if you use an arithmetic operator like + and the result is too large to fit into a 64-bit integer,
;; an exception is thrown. You can use +' to indicate that you would rather overflow into Clojure's slower,
;; arbitrary-precision bigint.
;;
;; (= (second (__ [2 3 2])) [2 5 5 2])
;;
;; (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
;;
;; (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
;;
;; (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))
;; solution 1:
(fn [sqn]
  (let [get-pt-seq (fn [sqn]
                     (map +' (concat '(0) sqn) (concat sqn '(0))))]
    (iterate get-pt-seq sqn)))
;; solution 2:
(fn [sqn]
  (iterate #(map +' `(0 ~@%) `(~@% 0)) sqn))

;; #146: Trees into tables
;;
;; Because Clojure's for macro allows you to "walk" over multiple sequences in a nested fashion,
;; it is excellent for transforming all sorts of sequences. If you don't want a sequence as your final output
;; (say you want a map), you are often still best-off using for, because you can produce a sequence and feed it into a map,
;; for example.
;; For this problem, your goal is to "flatten" a map of hashmaps.
;; Each key in your output map should be the "path"1 that you would have to take in the original map to get to a value,
;; so for example {1 {2 3}} should result in {[1 2] 3}. You only need to flatten one level of maps:
;; if one of the values is a map, just leave it alone.
;; 1 That is, (get-in original [k1 k2]) should be the same as (get result [k1 k2])
;;
;; (= (__ '{a {p 1, q 2}
;;          b {m 3, n 4}})
;;    '{[a p] 1, [a q] 2
;;      [b m] 3, [b n] 4})
;;
;; (= (__ '{[1] {a b c d}
;;          [2] {q r s t u v w x}})
;;    '{[[1] a] b, [[1] c] d,
;;      [[2] q] r, [[2] s] t,
;;      [[2] u] v, [[2] w] x})
;;
;; (= (__ '{m {1 [a b c] 3 nil}})
;;    '{[m 1] [a b c], [m 3] nil})
;; solution 1:
(fn [m]
  (apply (partial assoc {})
         (mapcat (fn [[okey nval]]
                   (mapcat (fn [[ikey value]]
                          [[okey ikey] value])
                        nval))
                 m)))
;; solution 2:
#(into {} (for [[k1 v1] % [k2 v2] v1] [[k1 k2] v2]))

;; #153: Pairwise Disjoint Sets
;;
;; Given a set of sets, create a function which returns true if no two of those sets have any elements in common1
;; and false otherwise. Some of the test cases are a bit tricky, so pay a little more attention to them.
;; 1Such sets are usually called pairwise disjoint or mutually disjoint.
;;
;; (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
;;    true)
;;
;; (= (__ #{#{:a :b :c :d :e}
;;          #{:a :b :c :d}
;;          #{:a :b :c}
;;          #{:a :b}
;;          #{:a}})
;;    false)
;;
;; (= (__ #{#{[1 2 3] [4 5]}
;;          #{[1 2] [3 4 5]}
;;          #{[1] [2] 3 4 5}
;;          #{1 2 [3 4] [5]}})
;;    true)
;;
;; (= (__ #{#{'a 'b}
;;          #{'c 'd 'e}
;;          #{'f 'g 'h 'i}
;;          #{''a ''c ''f}})
;;    true)
;;
;; (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
;;          #{#{:x :y :z} #{:x :y} #{:z} #{}}
;;          #{'[:x :y :z] [:x :y] [:z] [] {}}})
;;    false)
;;
;; (= (__ #{#{(= "true") false}
;;          #{:yes :no}
;;          #{(class 1) 0}
;;          #{(symbol "true") 'false}
;;          #{(keyword "yes") ::no}
;;          #{(class '1) (int \0)}})
;;    false)
;;
;; (= (__ #{#{distinct?}
;;          #{#(-> %) #(-> %)}
;;          #{#(-> %) #(-> %) #(-> %)}
;;          #{#(-> %) #(-> %) #(-> %)}})
;;    true)
;;
;; (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
;;          #{'+ '* mapcat (comment mapcat)}
;;          #{(do) set contains? nil?}
;;          #{, , , #_, , empty?}})
;;    false)
;; solution 1:
(fn [set]
  (= (count (apply clojure.set/union set)) (reduce + (map count set))))

;; #46: Flipping out
;;
;; Write a higher-order function which flips the order of the arguments of an input function.
;;
;; (= 3 ((__ nth) 2 [1 2 3 4 5]))
;;
;; (= true ((__ >) 7 8))
;;
;; (= 4 ((__ quot) 2 8))
;;
;; (= [1 2 3] ((__ take) [1 2 3 4 5] 3))
;; solution 1:
(fn [f]
  (fn [& args] (apply f (reverse args))))


;; #44: Rotate Sequence
;;
;; Write a function which can rotate a sequence in either direction.
;;
;; (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
;;
;; (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
;;
;; (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
;;
;; (= (__ 1 '(:a :b :c)) '(:b :c :a))
;;
;; (= (__ -4 '(:a :b :c)) '(:c :a :b))
;; solution 1:
(fn [offset sqn]
  (let [off-set-mod (mod offset (count sqn))]
    (concat (drop off-set-mod sqn) (take off-set-mod sqn))))

;; 08/02/2019
;; #43: Reverse Interleave
;;
;; Write a function which reverses the interleave process into x number of subsequences.
;;
;; (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
;;
;; (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
;;
;; (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))
;; solution 1:
(fn [sqn n]
  (for [i (range n)]
    (take-nth n (drop i sqn))))

;; #50: Split by Type
;;
;; Write a function which takes a sequence consisting of items with different
;; types and splits them up into a set of homogeneous sub-sequences.
;; The internal order of each sub-sequence should be maintained, but the sub-sequences themselves
;; can be returned in any order (this is why 'set' is used in the test cases).
;;
;; (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
;;
;; (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
;;
;; (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})
;; solution 1:
(fn [sqn]
  (vals (group-by type sqn)))

;;#55: Count Occurrences
;;
;; Write a function which returns a map containing the number of occurences of each distinct item in a sequence.
;;
;; (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
;;
;; (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
;;
;; (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})
;; solution 1:
((fn [sqn]
   (reduce (partial merge-with +) (for [e sqn] {e 1}))) [1 1 2 1 2 3])

;; #56: Find Distinct Items
;;
;; Write a function which removes the duplicates from a sequence. Order of the items must be maintained.
;;
;; (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
;;
;; (= (__ [:a :a :b :b :c :c]) [:a :b :c])
;;
;; (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
;;
;; (= (__ (range 50)) (range 50))
;; solution 1:
(fn [sqn]
  (reduce #(if ((set %1) %2)
              %1
              (conj %1 %2)) [] sqn))

;; #58: Function Composition
;;
;; Write a function which allows you to create function compositions.
;; The parameter list should take a variable number of functions, and create a function that applies them from right-to-left.
;;
;; (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
;;
;; (= 5 ((__ (partial + 3) second) [1 2 3 4]))
;;
;; (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
;;
;; (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))
;; solution 1:
(fn [& fns]
  (fn [& params]
    (reduce #(%2 %1) (apply (last fns) params) (rest (reverse fns)))))
;; solution 2:
(fn [& fns]
  (reduce (fn [f g] #(f (apply g %&))) fns))

;; 08/09/2019
;; #54: Partition a Sequence
;;
;; Write a function which returns a sequence of lists of x items each. Lists of less than x items should not be returned.
;;
;; (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
;;
;; (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
;;
;; (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))
;; solution 1:
(fn [n sqn]
   (let [my-partition
         (fn foo [n sqn]
           (let [p (take n sqn)]
             (if (= n (count p))
               (cons p (foo n (nthrest sqn n))))))]
     (filter #(= n (count %)) (my-partition n sqn))))
;; solution 2:
(fn p [n c]
  (when (and (seq c) (>= (count c) n))
    (cons (take n c) (p n (drop n c)))))

;; #59: Juxtaposition
;;
;; Take a set of functions and return a new function that takes a variable
;; number of arguments and returns a sequence containing the result of applying
;; each function left-to-right to the argument list.
;;
;; (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
;;
;; (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
;;
;; (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))
;; solution 1:
(fn [& fns]
  (fn [& args]
    (map #(apply % args) fns)))

;; #70: Word Sorting
;;
;; Write a function that splits a sentence up into a sorted list of words.
;; Capitalization should not affect sort order and punctuation should be ignored.
;;
;; (= (__  "Have a nice day.")
;;    ["a" "day" "Have" "nice"])
;;
;; (= (__  "Clojure is a fun language!")
;;    ["a" "Clojure" "fun" "is" "language"])
;;
;; (= (__  "Fools fall for foolish follies.")
;;    ["fall" "follies" "foolish" "Fools" "for"])
;; solution 1:
(fn [s]
  (sort #(compare (clojure.string/lower-case %1) (clojure.string/lower-case %2)) (clojure.string/split s #"[ .!]")))
;; solution 2:
(fn [s]
  (sort-by clojure.string/lower-case
           (re-seq #"[A-Za-z0-9]+" s)))

;; #67: Prime Numbers
;;
;; Write a function which returns the first x number of prime numbers.
;;
;; (= (__ 2) [2 3])
;;
;; (= (__ 5) [2 3 5 7 11])
;;
;; (= (last (__ 100)) 541)
;; solution 1:
(fn [num] 
  (let [prime? (fn [number]
                 (if (= 0 (mod number 2))
                   false
                   (not-any? zero? (map mod (repeat number) (range 3 (/ number 2) 2)))))]
    (cons 2 (take (dec num) (filter prime? (iterate inc 2))))))

; #65: Black Box Testing
;
; Clojure has many sequence types, which act in subtly different ways. 
; The core functions typically convert them into a uniform "sequence" type and work with them that way, 
; but it can be important to understand the behavioral and performance differences so that you know which kind is appropriate for your application.
;
; Write a function which takes a collection and returns one of :map, :set, :list, or :vector - describing the type of collection it was given.
; You won't be allowed to inspect their class or use the built-in predicates like list? - the point is to poke at them and understand their behavior.
; 
; (= :map (__ {:a 1, :b 2}))
; 
; (= :list (__ (range (rand-int 20))))
; 
; (= :vector (__ [1 2 3 4 5 6]))
; 
; (= :set (__ #{10 (rand-int 5)}))
; 
; (= [:map :set :vector :list] (map __ [{} #{} [] ()]))
; solution 1:
(fn [coll]
  (let [base (empty coll)]
    (condp = base
      #{} :set
      {} :map
      (if (reversible? coll) :vector :list))))

; #74: Filter Perfect Squares
;
; Given a string of comma separated integers, write a function which returns a new comma separated string
; that only contains the numbers which are perfect squares.
;
; (= (__ "4,5,6,7,8,9") "4,9")
;
; (= (__ "15,16,25,36,37") "16,25,36")
; solution 1
(fn [s]
  (->> s
       (#(clojure.string/split % #","))
       (map read-string)
       (filter #(let [root (Math/sqrt %)] (== root (int root))))
       (clojure.string/join ",")))
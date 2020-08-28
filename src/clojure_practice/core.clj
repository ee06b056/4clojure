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

; #76: Intro to Trampoline
;
; The trampoline function takes a function f and a variable number of parameters. 
; Trampoline calls f with any parameters that were supplied. If f returns a function, trampoline calls that function with no arguments. 
; This is repeated, until the return value is not a function, and then trampoline returns that non-function value. 
; This is useful for implementing mutually recursive algorithms in a way that won't consume the stack.
;
; #(bar (conj x y) y
; (= __
;    (letfn
;      [(foo [x y] #(bar (conj x y) y))
;       (bar [x y] (if (> (last x) 10)
;                    x
;                    #(foo x (+ 2 y))))]
;      (trampoline foo [] 1)))
[1 3 5 7 9 11]

; #77: Anagram Finder
;
; Write a function which finds all the anagrams in a vector of words. 
; A word x is an anagram of word y if all the letters in x can be rearranged in a different order to form y. 
; Your function should return a set of sets, where each sub-set is a group of words which are anagrams of each other. 
; Each sub-set should have at least two words. Words without any anagrams should not be included in the result.
; 
; (= (__ ["meat" "mat" "team" "mate" "eat"])
;    #{#{"meat" "team" "mate"}})
; 
; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
; (= (__ ["veer" "lake" "item" "kale" "mite" "ever"])
;    #{#{"veer" "ever"} #{"lake" "kale"} #{"mite" "item"}})
(fn [words]
  (set (map set (filter #(> (count %) 1) (vals (group-by sort words))))))

; #60: Sequence Reductions
;
; Write a function which behaves like reduce, but returns each intermediate value of the reduction. 
; Your function must accept either two or three arguments, and the return sequence must be lazy.
; 	
; (= (take 5 (__ + (range))) [0 1 3 6 10])
; 	
; (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
; 
; (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)
(defn my-reductions
  ([f col]
   (my-reductions f (first col) (rest col)))
  ([f acc col]
   (cons acc
         (lazy-seq
          (when (seq col)
            (my-reductions f (f acc (first col)) (rest col)))))))

;; #80: Perfect Numbers
;; 
;; A number is "perfect" if the sum of its divisors equal the number itself. 6 is a perfect number 
;; because 1+2+3=6. Write a function which returns true for perfect numbers and false otherwise.
;; 
;; (= (__ 6) true)
;; 
;; (= (__ 7) false)
;; 
;; (= (__ 496) true)
;; 
;; (= (__ 500) false)
;; 
;; (= (__ 8128) true)
(fn [n]
  (= n
     (reduce
      +
      (filter #(zero? (mod n %)) (range 1 n)))))

;; #69: Merge with a Function
;;
;; Write a function which takes a function f and a variable number of maps. 
;; Your function should return a map that consists of the rest of the maps conj-ed onto the first. 
;; If a key occurs in more than one map, the mapping(s) from the latter (left-to-right) should be 
;; combined with the mapping in the result by calling (f val-in-result val-in-latter)
;; 
;; (= (__ * {:a 2, :b 3, :c 4} {:a 2} {:b 2} {:c 5})
;;    {:a 4, :b 6, :c 20})
;; 
;; (= (__ - {1 10, 2 20} {1 3, 2 10, 3 15})
;;    {1 7, 2 10, 3 15})
;; 
;; (= (__ concat {:a [3], :b [6]} {:a [4 5], :c [8 9]} {:b [7]})
;;    {:a [3 4 5], :b [6 7], :c [8 9]})
(fn [f & maps]
  (reduce
   (fn [m1 m2]
     (reduce
      (fn [accm [k v]]
        (if (contains? accm k)
          (update-in accm [k] f v)
          (assoc accm k v)))
      m1
      m2))
   maps))

;; #102: intoCamelCase
;; 
;; When working with java, you often need to create an object with fieldsLikeThis,
;; but you'd rather work with a hashmap that has :keys-like-this until it's time to convert. 
;; Write a function which takes lower-case hyphen-separated strings and converts them to camel-case strings.
;; 
;; (= (__ "something") "something")
;; 
;; (= (__ "multi-word-key") "multiWordKey")
;; 
;; (= (__ "leaveMeAlone") "leaveMeAlone")
(fn [s]
  (let [ss (clojure.string/split s #"-")]
    (apply str (first ss) (map clojure.string/capitalize (rest ss)))))

; #75: Euler's Totient Function
;
; Two numbers are coprime if their greatest common divisor equals 1. 
; Euler's totient function f(x) is defined as the number of positive 
; integers less than x which are coprime to x. The special case f(1) equals 1. 
; Write a function which calculates Euler's totient function.
;
; (= (__ 1) 1)
;
; (= (__ 10) (count '(1 3 7 9)) 4)
;
; (= (__ 40) 16)
;
; (= (__ 99) 60)
(fn [n]
  (let [isCoprime (fn [a b]
                    (let [gcd (fn gcd [a b]
                                (if (= 0 (mod a b))
                                  b
                                  (gcd b (mod a b))))]
                      (= 1 (gcd a b))))]
    (if (= 1 n)
      1
      (count (filter (partial isCoprime n) (range 1 n))))))

; #86: Happy numbers
;
; Happy numbers are positive integers that follow a particular formula: 
; take each individual digit, square it, and then sum the squares to get
 ; a new number. Repeat with the new number and eventually, you might get 
; to a number whose squared sum is 1. This is a happy number. An unhappy 
; number (or sad number) is one that loops endlessly. Write a function that 
; determines if a number is happy or not.
;
; (= (__ 7) true)
;
; (= (__ 986543210) true)
;
; (= (__ 2) false)
;
; (= (__ 3) false)
(fn [n]
  (let [get-all-digits (fn get-all-digits [n]
                         (let [q (quot n 10)
                               m (mod n 10)]
                           (cons m (when (> q 0) (get-all-digits q)))))
        sqr-sum (fn [digits]
                  (apply + (map #(* % %) digits)))]
    (loop [n n
           val-history #{}]
      (let [s (sqr-sum (get-all-digits n))]
        (cond
          (= 1 s) true
          (val-history s) false
          :else (recur s (conj val-history s)))))))

; #78: Reimplement Trampoline
;
; Reimplement the function described in "Intro to Trampoline".
;
; (= (letfn [(triple [x] #(sub-two (* 3 x)))
;           (sub-two [x] #(stop?(- x 2)))
;           (stop? [x] (if (> x 50) x #(triple x)))]
;     (__ triple 2))
;   82)
;
; (= (letfn [(my-even? [x] (if (zero? x) true #(my-odd? (dec x))))
;           (my-odd? [x] (if (zero? x) false #(my-even? (dec x))))]
;     (map (partial __ my-even?) (range 6)))
;   [true false true false true false])
(fn [f & args]
  (let [res (apply f args)]
    (if (fn? res)
      (loop [f (res)]
        (if (fn? f)
          (recur (f))
          f))
      res)))

#(->> (%1 %2)
      (iterate (fn [f] (f)))
      (drop-while fn?)
      (first))

; #115: The Balance of N
;
; A balanced number is one whose component digits have the same sum on the 
; left and right halves of the number. Write a function which accepts an 
; integer n, and returns true iff n is balanced.
;
; (= true (__ 11))
;
; (= true (__ 121))
;
; (= false (__ 123))
;
; (= true (__ 0))
;
; (= false (__ 88099))
;
; (= true (__ 89098))
;
; (= true (__ 89089))
;
; (= (take 20 (filter __ (range)))
;    [0 1 2 3 4 5 6 7 8 9 11 22 33 44 55 66 77 88 99 101])
(fn [n]
  (let [get-all-digits (fn get-all-digits [n]
                         (let [q (quot n 10)
                               m (mod n 10)]
                           (cons m (when (> q 0) (get-all-digits q)))))
        all-digits (get-all-digits n)
        half-length (quot (count all-digits) 2)]
    (= (apply + (take half-length all-digits))
       (apply + (drop (- (count all-digits) half-length) all-digits)))))

; #85: Power Set
;
; Write a function which generates the power set of a given set. 
; The power set of a set x is the set of all subsets of x, including the empty set and x itself.
;
; (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
;
; (= (__ #{}) #{#{}})
;
; (= (__ #{1 2 3})
;    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
;
; (= (count (__ (into #{} (range 10)))) 1024)
(fn [s]
  (reduce (fn [acc e]
            (into acc (map #(conj % e) acc)))
          #{#{}}
          s))

; #98: Equivalence Classes
;
; A function f defined on a domain D induces an equivalence relation on D, 
; as follows: a is equivalent to b with respect to f if and only if (f a) 
; is equal to (f b). Write a function with arguments f and D that computes 
; the equivalence classes of D with respect to f.
;
; (= (__ #(* % %) #{-2 -1 0 1 2})
;    #{#{0} #{1 -1} #{2 -2}})
;
; (= (__ #(rem % 3) #{0 1 2 3 4 5 })
;    #{#{0 3} #{1 4} #{2 5}})
;
; (= (__ identity #{0 1 2 3 4})
;    #{#{0} #{1} #{2} #{3} #{4}})
;
; (= (__ (constantly true) #{0 1 2 3 4})
;    #{#{0 1 2 3 4}})
(fn [f col]
  (->> (group-by f col)
       vals
       (map #(into #{} %))
       (into #{})))

; #105: Identify keys and values
;
; Given an input sequence of keywords and numbers, 
; create a map such that each key in the map is a keyword, 
; and the value is a sequence of all the numbers (if any) 
; between it and the next keyword in the sequence.
;
; (= {} (__ []))
;
; (= {:a [1]} (__ [:a 1]))
;
; (= {:a [1], :b [2]} (__ [:a 1, :b 2]))
;
; (= {:a [1 2 3], :b [], :c [4]} (__ [:a 1 2 3 :b :c 4]))
(fn kv [acc k [v & vs]]
  (cond
    (nil? v) acc
    (keyword? v) (kv (assoc acc v []) v vs)
    :else (kv (update-in acc [k] conj v) k vs))) {} nil

; #137: Digits and bases
;
; Write a function which returns a sequence of digits of a 
; non-negative number (first argument) in numerical system 
; with an arbitrary base (second argument). Digits should be 
; represented with their integer values, e.g. 15 would be [1 5] 
; in base 10, [1 1 1 1] in base 2 and [15] in base 16.
;
; (= [1 2 3 4 5 0 1] (__ 1234501 10))
;
; (= [0] (__ 0 11))
;
; (= [1 0 0 1] (__ 9 2))
;
; (= [1 0] (let [n (rand-int 100000)](__ n n)))
;
; (= [16 18 5 24 15 1] (__ Integer/MAX_VALUE 42))
(fn [n b]
  (if (> n 0)
    (loop [n n
           b b
           res []]
      (if (> n 0)
        (recur (quot n b) b (conj res (mod n b)))
        (reverse res)))
    [0]))
; better one:
((fn base [n b]
  (if (>= n b)
    (conj (base (quot n b) b) (mod n b))
    [n])) 9 2)

;; #144: Oscilrate
;; 
;; Write an oscillating iterate: a function that takes an initial value 
;; and a variable number of functions. It should return a lazy sequence 
;; of the functions applied to the value in order, restarting from the 
;; first function after it hits the end.
;;
;; (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
;;
;; (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
;;
;; (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])
(fn f [n & [f1 & fs]]
  (lazy-seq
   (cons n (apply f (f1 n) (concat fs (list f1))))))

;; #158: Decurry
;;
;; Write a function that accepts a curried function of unknown arity n. 
;; Return an equivalent function of n arguments.
;;
;; (= 10 ((__ (fn [a]
;;              (fn [b]
;;                (fn [c]
;;                  (fn [d]
;;                    (+ a b c d))))))
;;        1 2 3 4))
;;
;; (= 24 ((__ (fn [a]
;;              (fn [b]
;;                (fn [c]
;;                  (fn [d]
;;                    (* a b c d))))))
;;        1 2 3 4))
;;
;; (= 25 ((__ (fn [a]
;;              (fn [b]
;;                (* a b))))
;;        5 5))
(fn [f]
  (fn [& args]
    (loop [f f
           args args]
      (if (seq args)
        (recur (f (first args)) (rest args))
        f))))
;; better one:
(fn [f]
  (fn [& args]
    (reduce #(%1 %2) f args)))

;; #110: Sequence of pronunciations
;;
;; Write a function that returns a lazy sequence of "pronunciations" 
;; of a sequence of numbers. A pronunciation of each element in the 
;; sequence consists of the number of repeating identical numbers 
;; and the number itself. For example, [1 1] is pronounced as [2 1] 
;; ("two ones"), which in turn is pronounced as [1 2 1 1] ("one two, one one").
;; 
;; Your function should accept an initial sequence of numbers, 
;; and return an infinite lazy sequence of pronunciations, 
;; each element being a pronunciation of the previous element.
;;
;; (= [[1 1] [2 1] [1 2 1 1]] (take 3 (__ [1])))
;;
;; (= [3 1 2 4] (first (__ [1 1 1 4 4])))
;;
;; (= [1 1 1 3 2 1 3 2 1 1] (nth (__ [1]) 6))
;;
;; (= 338 (count (nth (__ [3 2]) 15)))
(fn lazy-sp [col]
  (let [next-col (->> col
                     (partition-by identity)
                      (mapcat #(list (count %) (first %))))]
    (lazy-seq
     (cons next-col (lazy-sp next-col)))))

;; #108: Lazy Searching
;;
;; Given any number of sequences, each sorted from smallest 
;; to largest, find the smallest single number which appears 
;; in all of the sequences. The sequences may be infinite, so 
;; be careful to search lazily.
;;
;; (= 3 (__ [3 4 5]))
;;
;; (= 4 (__ [1 2 3 4 5 6 7] [0.5 3/2 4 19]))
;;
;; (= 7 (__ (range) (range 0 100 7/6) [2 3 5 7 11 13]))
;;
;; (= 64 (__ (map #(* % % %) (range)) ;; perfect cubes
;;           (filter #(zero? (bit-and % (dec %))) (range)) ;; powers of 2
;;           (iterate inc 20))) ;; at least as large as 20
(fn [& cols]
  (if (= 1 (count cols))
    (first (first cols))
    (let [heads (map first cols)
          largest (apply max heads)]
      (if (apply = heads)
        largest
        (recur (map (fn [col] (drop-while #(< % largest) col)) cols))))))

;; #93: Partially Flatten a Sequence
;; 
;; Write a function which flattens any 
;; nested combination of sequential things 
;; (lists, vectors, etc.), but maintains the 
;; lowest level sequential items. The result 
;; should be a sequence of sequences with only one level of nesting.
;;
;; (= (__ [["Do"] ["Nothing"]])
;;    [["Do"] ["Nothing"]])
;;
;; (= (__ [[[[:a :b]]] [[:c :d]] [:e :f]])
;;    [[:a :b] [:c :d] [:e :f]])
;;
;; (= (__ '((1 2)((3 4)((((5 6)))))))
;;    '((1 2)(3 4)(5 6)))
(fn pfs [tree]
  (if (every? sequential? tree)
    (mapcat pfs tree)
    [tree]))

;; #114: Global take-while
;;
;; take-while is great for filtering sequences, but it limited: 
;; you can only examine a single item of the sequence at a time. 
;; What if you need to keep track of some state as you go over the sequence?
;;
;; Write a function which accepts an integer n, a predicate p, and a sequence. 
;; It should return a lazy sequence of items in the list up to, but not including, 
;; the nth item that satisfies the predicate.
;;
;; (= [2 3 5 7 11 13]
;;    (__ 4 #(= 2 (mod % 3))
;;          [2 3 5 7 11 13 17 19 23]))
;;
;; (= ["this" "is" "a" "sentence"]
;;    (__ 3 #(some #{\i} %)
;;          ["this" "is" "a" "sentence" "i" "wrote"]))
;;
;; (= ["this" "is"]
;;    (__ 1 #{"a"}
;;          ["this" "is" "a" "sentence" "i" "wrote"]))
(fn g-take-while [n pred col]
  (lazy-seq
   (when (and (> n 0) (seq col))
     (cond
       (and (pred (first col)) (> n 1))
       (cons (first col) (g-take-while (dec n) pred (rest col)))
       
       (not (pred (first col)))
       (cons (first col) (g-take-while n pred (rest col)))))))

(comment
  "experiment space"
  ((fn g-take-while [n pred col]
     (lazy-seq
      (when (and (> n 1) (seq col))
        (prn "n: "n "seq: " col)
        (if (pred (first col))
          (cons (first col) (g-take-while (dec n) pred (rest col)))
          (cons (first col) (g-take-while n pred (rest col)))))))
   4
   #(= 2 (mod % 3))
   [2 3 4 5 6 7 8 9 10 11 12 13 14 15])
  (let [pfs (fn pfs [cols]
              (map (fn [col]
                        (if (not-any? sequential? col)
                          col
                          (pfs col)))
                      cols))]
    (pfs [["do"] [["nothing"]]]))
  (let [flatten (fn flatten [col]
                  (if (every? sequential? col)
                    (mapcat flatten col)
                    [col]))]
    (flatten [[:a] [[:b]]]))
  ((fn f [tree] (if (every? (complement sequential?) tree)
                [tree]
                (mapcat f tree))) [:a [:b]])
  ((fn [col] (map (fn [a] (if (coll? a) (first a) a)) col)) [:a [:b]])
  (not-any? sequential? [["nothing"]])
  (map identity [["do"] ["nothing"]])
  (not-any? sequential? ["do"])
  (seq? "a")
  (sequential? '(1))
  (every? (comp not sequential?) [1 2 3])
  (not-any? sequential? [1 2 3])
  (take 10 (iterate inc 20))
  (lazy-seq
   )
  (->> '(1 1 1 2 2 1)
       (partition-by identity)
       (mapcat #(list (count %) (first %))))
  (partition-by identity [1])
  (cons 1 (cons 2 nil))
  (let [f (fn f [n & [f1 & fs]]
            (lazy-seq
             (cons n (apply f (f1 n) (concat fs (list f1))))))]
    (take 5 (f 1 inc dec)))
  (let [f (fn ff [n & [f1 & fs]]
            (let [tmp (f1 n)]
              (lazy-seq (cons tmp (apply ff tmp (concat fs '(f1)))))))]
    (take 5 (f 3 inc dec)))
  (let [f (fn f [a]
            (lazy-seq (cons (inc a) (f (inc a)))))]
    (take 5 (f 1)))
  (cons 4 '(1 2 3))
  (conj '(1 2 3) 4)
  (concat '(1 2 3) '(4))
  (partition-by keyword? [:a 1 2 3 :b :c 4])
  (conj '() 1)
  (quot 123542312 2)
  (conj [1] 2)
  (reverse [1 2 3])
  (let [f #(* % %)
        col #{0 1 2 -1 -2}]
    (into #{} (map #(into #{} %) (vals (group-by f col)))))
  (rem 0 3)
  (into #{#{}} '(#{:a} #{:a 1}))
  (range 1 3)
  (conj #{} :a)
  (conj #{} :a)
  (concat #{1} #{:a})
  (reduce #(prn %2)
          #{}
          #{:a 1 :b "bv"})
  (seq '())
  (let [f1 #(->> (%1 %2))]
    (f1 + 1))
  (quot 1 2)
  (mod 11 10)
  (mod 13 5)
  (mod 5 3)
  (mod 3 2)
  (mod 2 1)
  (mod 1 0)
  (let [get-all-digits (fn get-all-digits [n]
                         (let [q (quot n 10)
                               m (mod n 10)]
                           (cons m (when (> q 0) (get-all-digits q)))))]
    (get-all-digits 0))
  (quot 123 10)
  (mod 123 10)
  (let [gcd (fn gcd [a b]
              (if (= 0 (mod a b))
                b
                (gcd b (mod a b))))
        isCoprime (fn [a b]
                    (= 1 (gcd a b)))]
    (gcd 4 2)))
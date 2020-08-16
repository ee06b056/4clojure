(ns clojure-practice.protocol)

; (defprotocol Bazzer
;   "This is an interface that state a `baz` method should be implemented"
;   (baz [this] [a b]))

; (defprotocol Barrrr
;   (bar [this] [a b]))

; (extend-protocol Bazzer
;   Object
;   (baz
;     ([a] 1)
;     ([a b] 2))

;   Foo
;   (baz
;    [this] "from extend protocol")
  
;   String
;   (baz
;     ([a] "from string one arg")
;     ([a b] "from string two args")))

; (extend-protocol Barrrr
;   String
;   (bar
;     [a] "from barrrr one arg")
;   Foo
  
;   Object
;   (bar
;     ([a] "from barrrr one arg")
;     ([a b] "from barrr two args")))

; #_(extend-type String Barrrr
;              (bar [a] "from extend type one arg")
;              (bar [a b] "from extend type two args"))

; (prn (baz "one arg return 1"))
; (prn (baz "string" 2))
; (prn (bar "libo"))


; (defrecord Foo [a b])

; (def foo (->Foo :bar :baz))

; (def foobar (map->Foo {:c 1 :d 2}))

; (baz foo)
; (type foo)

; (baz foobar)


; (def basil (Foo. "hai" "hi"))

; (baz basil)

; (defprotocol MyInterface
;   (foo [this])
;   (bar [this] [this x] [this x y] [this x y z]))

; (deftype MyClass [a b c]
;   MyInterface
;   (foo [this] a)
;   (bar [this] b)
;   (bar [this x] (+ c x))
;   (bar [this x y] (+ c x y))
;   (bar [this x y z] (+ c x y z)))

; (def obj (MyClass. 1 2 3))

; (foo obj)

; (defrecord A [x y])
; (def a (A. 1 2))
; ; (def aa (->A 1 2))
; ; (def aaa (map->A {:x 1 :y 2}))
; ; [a aa aaa]
; ; (map->A {:a 1 :b 3 :z 45})


; (deftype B [x y])
; (->B 1 2)
; (B. 4 5)

; (type (B/getBasis))
; 

(defmulti full-moon-behavior
  (fn [were-creature]
    (:were-type were-creature)))

(defmethod full-moon-behavior :wolf
  [were-creature]
  (str "good"))

(defprotocol Psychodynamics
  (thoughts [x] "the data type")
  (feelings-about [x] [x y] "others"))

 (extend-type java.lang.String
   Psychodynamics
   (thoughts [x] "haha")
   (feelings-about
     ([x] "hehe")
     ([x y] "hoho")))

(extend-protocol Psychodynamics
  java.lang.String
  (thoughts [x] "haha")
  (feelings-about
    ([x] "hehe")
    ([x y] "hoho")))

(defrecord WereWolf [name title])

(WereWolf. "libo" "coder")
 
 (->WereWolf "libo" "coder")
 
 (map->WereWolf {:name "libo" :title "coder"})

(defprotocol Drives
  (drive [this throttle]))

(defrecord Car [top-speed]
  Drives
  (drive [_ throttle] (str "spin wheels " (* top-speed throttle))))
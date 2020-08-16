(ns clojure-practice.async
  (:require [clojure.core.async :refer [thread go go-loop >! <! timeout chan close! >!! <!! alts! alts!! dropping-buffer alt! alt!! put! pipe mult tap]]))

(go-loop [second 1]
  (<! (timeout 1000))
  (println "wait for one second")
  (<! (timeout 1000))
  (println "wait for another second" second)
  (when (< second 10) 
    (recur (inc second))))

(let [c (chan)]
  (close! c))

(let [c (chan 10)]
  (>!! c "hello")
  (assert (= "hello" (<!! c)))
  (close! c))

(let [c (chan)]
  (thread (>!! c "hello"))
  (assert (= "hello" (<!! c)))
  (close! c))

(let [c (chan)]
  (thread (<!! c))
  (println (<!! (go (>! c "hello")))))

(let [c (chan)]
  (thread (>!! c "hello"))
  (println (<!! (go (<! c))))
  (close! c))

(let [c (chan)]
  (go (>! c "hello"))
  (assert (= "hello" (<!! (go (<! c)))))
  (close! c))

(let [c1 (chan)
      c2 (chan)]
  (thread (while true
            (let [[v ch] (alts!! [c1 c2])]
              (println "Read" v "from" ch))))
  (>!! c1 "hi")
  (>!! c2 "there"))

(let [c1 (chan)]
  (println (alts!! [c1 (timeout 1000)])))


(let [c (chan)]
  (go
   (alts! [c (timeout 2000)]))
  (>!! c "hello")
  )

(let [n 1000
      cs (repeatedly n chan)
      begin (System/currentTimeMillis)]
  (doseq [c cs] (go (>! c "hi")))
  (dotimes [_ n]
    (let [[v _] (alts!! cs)]
      (assert (= "hi" v))))
  (println "read" n "msgs in" (- (System/currentTimeMillis) begin) "ms"))

(let [t (timeout 1000)])

(let [c (chan)]
  (go-loop [t 100]
    (>! c "hello")
    (println "not blocking in" t "times")
    (when (pos? t)
      (recur (dec t)))))

(let [result-chan (chan)
      error-chan (chan)
      dont-care-chan (chan)]
  (go-loop [times 3]
    (alt!
      result-chan ([result] (println "result: " result))
      error-chan ([error] (println "error: " error))
      dont-care-chan (println "dont care"))
    (when (> times 2) (recur (dec times))))
  (put! result-chan "some result")
  (put! error-chan "some error")
  (put! dont-care-chan "dont care"))

(let [gochan (go 1)]
  (<!! gochan))

(let [trade-chn (chan)]
  (go-loop []
    (<! (timeout 1000))
    (println (<! trade-chn))
    (recur))
  (go 
    (let [timeout-ch (timeout 2000)
          trade 100]
      (-> 
       (alt!
         [[trade-chn trade]] :sent
         timeout-ch :timed-out)
       println))))

(let [cchan (chan)
      timeout-ch (timeout 2000)]
  (go
    (alt!
      cchan ([m] (println "cchan: " m))
      timeout-ch ([m] (println "timeout: " m))))
  (when cchan (println "cchan true"))
  (close! cchan)
  (when (not cchan) (println "cchan false")))

(let [c (chan)]
  (put! c "foo")
  (println (alts!! [c (timeout 2000)]))
  (println (alts!! [c (timeout 2000)])))

(let [chans (partition-all 2 (interleave ["bob" "jane" "guygirl122"] (for [_ (range 3)] (chan))))
      [owner port] (rand-nth chans)
      chans-only (mapv second chans)]
  (go
    (<! (timeout 1500))
    (>! port (str owner ": First!!")))
  (let [[v p] (alts!! chans-only)]
    (println "message: " v "\nfrom object: " p)))


(let [fake-fetch (fn [] (thread
                          (Thread/sleep 5000)
                          "ready"))]
  (go (println (<! (fake-fetch)))))

(let [c1 (chan 1)
      c2 (chan 2)]
  (>!! c1 42)
  (>!! c2 44)
  (thread
    (let [[v c] (alt!! [c1] ([m] [m c1])
                       [c2] [:c2 c2])]
      (println "Value: " v)
      (println "chan 1? " (= c1 c))
      (println "chan 2? " (= c2 c)))))

(let [f (fn [x ch] (go (Thread/sleep (rand 100))
                       (>! ch x)))
      a (chan)
      b (chan)
      c (chan)]
  (f 1 a)
  (f 2 b)
  (f 3 c)
  
  (let [[n ch2] (alts!! [a b c]
                       :default 42
                       :priority true)]
    (println "result: " n)))

(let [c1 (chan)
      c2 (chan)]
  (pipe c1 c2)
  (thread
    (>!! c1 "hello"))
  (<!! c2)
  (thread
    (>!! c1 "hello again"))
  (<!! c2)
  (close! c1))

(let [sz 20
      c (chan sz)
      mult-c (mult c)
      cx (chan sz)
      cy (chan sz)
      cz (chan sz)]
  (tap mult-c cx)
  (tap mult-c cy)
  (tap mult-c cz)
  (put! c "send to all")
  (println (<!! cx) (<!! cy) (<!! cz)))
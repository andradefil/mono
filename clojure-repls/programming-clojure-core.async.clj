(require '[clojure.core.async :as a])

;; rationale
;; https://clojure.org/reference/async

;; what is the simplest way to work with async channel?
;; what is a channel?

;; a channel is a queue, that carry vals and support
;; multiple readers and writters

;; channels are created with chan

;; clojure, as we love is an expressive language
;;so, the program is clear when it is working with a chanell
;;and when the operation blocks, !! double bang.


;; When designing multi-threading programming with core.async
;;you only need to care about the information model as isolated process
;;input and output, not need to care about locks,semaphores,mutex etc.

;;maybe it's worth introduce this concept, also explaining about
;;isolation of process and a common pattern in Nubank, produce and consume
;;in the same service.

(a/chan)

;; very simple, what can I do with a channel....

;; imagine channel being a port,
;; only one can pass the port at a time
;; you can pass the port carying something

;; you can't make.

;; compare examples with anologies.
;; flip the perspectives

;; relevance


;; simple send/receive from chan
(def c (a/chan 1))

(a/>!! c "a")

;; alternative, put is async, don't coordinate, returns immediately
(a/put! c "a")
;; try to do this get an error, and check the function docs
(a/take! c)

(require '[clojure.repl :refer :all])
(doc a/take!)

;;returns nil
(a/take! c identity)

(a/take! c (fn [v] (println v)))

(doc a/offer!)
(doc a/poll!)

;; you can combine between pool and >!! blocking or non blocking puts
(a/>!! c "a")
(a/poll! c)

;; respects backpressure, but not blocks, returns true if put
(a/offer! c "a")

;; trying again implies in backpressure, blocks thread until consume
(a/>!! c "a")

;; here's opportunity for variation scenario, you can return
;; a timeout channel that can be added to or fallback default

;; what is the blocking thread? Is a long CPU operation considered
;; a blocking thread? No, only is blocking when your thread is in a
;; WAITING state

;; how can we know that?
;; opportunity for demonstrating using capturing a JFR of the application

;; operates on channels (own channels)
a/thread
a/go
a/go-loop

;; TODO, make some figures to clarify as analogy
;;- use internet with generative ai picture for anologies pictures

;; TODO, make the example for serialized log
;;- check again, how was this made on talk clojure/conj


;; TODO, make the example for interruption signal for worker
;; note: the talk will not focus on specify examples of usage
;;of core.async against other 'async' implementations but is worth
;;to consider mention that core.async doesn't force to change
;;the implementation semantics, so, the reasoning about the program
;;continue to be sequential.

;; in this implementation, why it returns only after we
;;put a value into the channel?
(require '[clojure.core.async :as a])
(defn work!
  [interrupt?]
  (a/go-loop []
    (if (a/<! interrupt?)
      (println "Interrupt!")
      (do
        (println ".")
        (Thread/sleep 1000)
        (recur)))))

(def control (a/chan 1))
(work! control)
(a/>!! control "stop")

;; mixing approaches of async get from a chan
(require '[clojure.core.async :as a])
(defn work!
  [interrupt?]
  (a/go-loop []
    (if (a/poll! interrupt?)
      (println "Interrupt!")
      (do
        (println ".")
        (Thread/sleep 1000)
        (recur)))))

(def control (a/chan 1))
(work! control)
(a/>!! control "stop")

(a/poll! control)


;; go/go-loop/thread
;;returns the our channel, with thread of control
;;note that while go, operates over a bounded thread pool
;;threads are real java threads, this operation have the thread overhead
;;but the threads are cached



;; https://clojure.org/reference/async
;; NOTE about go/blocks
;; Note that go blocks are multiplexed over a finite number of threads and should never be blocked, either by the use of a core.async blocking operation (like <!!) or by calling a blockable I/O operation like a network call. Doing so may effectively block all of the threads in the go block pool and prevent further progress.

;; creates a channel
a/chan

;; operates on given channels
a/put
a/take

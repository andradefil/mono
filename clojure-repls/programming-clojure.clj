;; programming Clojure book code snippets
;; running code to keep examples and explore as
;; book recommends to try everything in a repl
;; lets see how much effor this is going to take.
(+ 1 2)

(defn tail-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (fib next (+ current next) (dec n))))]
    (fib 0N 1N n)))

;; better but not great
(defn recur-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (recur next (+ current next) (dec n)))) ;;using recur do not consume the stack as it calculates
          ]
    (fib 0N 1N n)))

;; this illustrates the general pattern: wrap the recursve part of a function body
;; with lazy-seq to replace recursion with laziness
(defn lazy-seq-fibo
  ([]
   (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
      (cons n (lazy-seq-fibo b n))))))

(recur-fibo 9)
(recur-fibo 10000000)

;; lazy-seq-fibo works for small values
(take 10 (lazy-seq-fibo))

;; also works for large values
(rem (nth (lazy-seq-fibo) 100000) 1000)

;; considering reuse of existing sequence library functions
;; that return lazy sequences. Consider the use of iterate
(take 5 (iterate (fn [[a b]] [b (+ a b)]) [0 1]))

(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

;; avoinding realizing with lazy-sequences
(def lots-o-fibs (take 1000000000 (fibo)))
;; it will do only the necessary
(nth lots-o-fibs 100)

;; safe print billion of fibonacci
;; set the print length for repl reader
(set! *print-length* 100)
(take 100000000 (fibo))

;; holds the head (avoid!)
(def head-fibo (lazy-cat [0N 1N] (map + head-fibo (rest head-fibo))))

;; head-fibo works great for small fibonacci numbers
(take 10 head-fibo)

;; but not so well for huge  ons
(nth head-fibo 10000000)

;; the problem is that the top-level var head-fibo holds the head of the collection
;; this prevents the garbage collector from reclaiming elements of the sequence after
;; you've moved past those elements.

;; so any part of the fibonacci sequence that you actually use gts cachd for the life
;; of the value referenced by head-fibo, which is likely tobe the lifeof the program.


;; try to not implement lazy-sequences by hand
;; instead to use lazy functions built-in clojure

;; for example for the following problem
;; you're given a sequence of coin-toss results , where heads in :h and tails is :t

[:h t :t :h :h :h]

;; using recur
(defn count-heads-pairs [coll]
  (loop [cnt 0 coll coll]
    (if (empty? coll)
      cnt
      (recur (if (= :h (first coll) (second coll))
               (inc cnt)
               cnt)
             (rest coll)))))

(count-heads-pairs [:h :h :h :t :h])

(count-heads-pairs [:h :t :h :t :h])

;; handling obscureness with composing problems

;; overly complex, better approaches follow...
(defn by-pairs [coll]
  (let [take-pair (fn [c]
                    (when (next c) (take 2 c)))]
    (lazy-seq
     (when-let [pair (seq (take-pair coll))]
       (cons pair (by-pairs (rest coll)))))))

(by-pairs [:h :t :t :h :h :h])

;; now that we can think of the coin tossesas a sequence of pairs of results
;; it's easy to describe count-heads-pairs in English:

;; "Count the pairs of results that are all heads"

;; This English description translates directly into existing sequnce library
;; functions: "Count" is count, of course, and "that are all heads" suggests a
;; filter.
(defn count-heads-pairs [coll]
  (count (filter (fn [pair] (every? #(= :h %) pair))
                 (by-pairs coll))))

;; we can make things even simpler. Clojure already has a more general version
;; of by-pairs named partition
(doc partition)
;; partition breaks a collection into chunks of size size.
;; you could break a heads/tails vector into a sequence of pairs.
(partition 2 [:h :t :t :h :h :h])
;; isn't quite same as by-pairs, which yields overlapping pairs. But partition
;; can do overlaps, too. The optional step argument determines how far partition
;; moves down te collection before starting its next chunk. If not specified,
;; step is the same as size. To make prtition work like by-pairs, set size to 2
;; and set step to 1.
(partition 2 1 [:h :t :t :h :h :h])
(by-pairs [:h :t :t :h :h :h])

(require '[clojure.repl :refer :all])

(def ^{:doc "Count items matching a filter"}
  count-if (comp count filter))
;; cool! compose core-functions to create a new function
;; so, count-if will first filter and then count the results of the filter
(count-if odd? [1 2 3 4 5])

;; Finally, you can use count-if and partition to create a count-runs function
;; that's more general than count-heads-pairs.
(defn count-runs
  "Count runs of length n where pred is true in coll"
  [n pred coll]
  (count-if #(every? pred %) (partition n 1 coll)))
;; count-runs is a winning combination.
;; generic functions, generic problem, categorical problem

;; you can use it to count *pairs* of heads
;; nothing to related to key,value, instead, it considers a logic
;; to be applied to the entire pair.
(count-runs 2 #(= % :h) [:h :t :t :h :h :h])

;; but you can just as easily use it to count pairs of tails
(count-runs 2 #(= % :t) [:h :t :t :h :h :h])

;; Or, instead of pairs, how about runs of three heads in a row?
(count-runs 3 #(= % :h) [:h :t :t :h :h :h])

;; more composing
;; if you still want to have a function named count-heads-pairs
;; you can implement it in terms of count-runs
(def ^{:doc "Count runs of length two that are both heads"}
  count-heads-pairs (partial count-runs 2 #(= % :h)))


;; almost a curry
(defn faux-curry [& args] (apply partial partial args))

;; one use of curry is partial application. here is partial application
;; in Clojure.
(def add-3 (partial + 3))
(add-3 7)

;; since clojure functions can have variable-length arguments list
;; clojure can't know when all arguments are fixed
;; but you, the programmer, do know as you want, just invoke the function
(((faux-curry true?) (= 1 1)))

;; recursion revisited
;; TCO - tail call optimization
;; resource for call explicity TCO in recur and loop

;; amutual recurion occurs when the recursion bounces between
;; two or more functions. instead of A calls A calls A, we have
;; A calls B calls A again

(declare my-odd? my-even?)

(defn my-odd? [n]
  (if (= n 0)
    false
    (my-even? (dec n))))

(defn my-even? [n]
  (if (= n 0)
    true
    (my-odd? (dec n))))

;; verify my-odd? and my-even? work for small values
(map my-even? (range 10))
(map my-odd? (range 10))

;; my-odd? and my-even? consume stack frames proportional to the size
;; of their argument, so they will fail with large numbers

(my-even? (* 1000 1000 1000));; it throws StackOverflowError
;; in jvm running repl the function exceeded max stack frames
;; causing stack overflow

;; of course, odd/even can be implemented more efficiently without
;; recursion anyway. Clojure's implementation uses bit-and (bitwise and)

;; from core.clj
(defn even? [n] (zero? (bit-and n 1)))
(defn odd? [n] (not (even? n)))

;; Other recursive problems are not so simple and don't have an elegant
;; nonrecursive solution. We'll examine four approaches you can use
;; to solve such problems.

;; - converting to self-recursion
;; - trampoling a mutual recursion
;; - replacing recursion with lazyness
;; - shortcuting recursion with memoization

;; # mutual recursion
;; is often a nice way to model separate but related concepts 
;; For example, oddness and evenness are separate concepts
;; but clearly related to one another


;; you can convert a mutual recursion to a self-recursion by coming up
;; with a single abstraction that deals with multiple concepts simultaneously
;; For example you can think about eveness and oddness in terms of parity
(defn parity [n]
  (loop [n n par 0]
    (if (= n 0)
      par
      (recur (dec n) (- 1 par);; it switches set flag for current n
             ))))
;; test the parity works for small values
(map parity (range 10))

;; at this point you can trivially implement my-odd? and my-even?
;; in terms of parity
(defn my-even? [n] (= 0 (parity n)))
(defn my-odd? [n] (= 1 (parity n)))

;; # trampoline
;; is a technique for optimizing mutual recursion. A trampoline is like
;; an after-the-fact recur, imposed by the caller of a function instead
;; of the implementer.
;; since the caller can call more than one function inside a trampoline
;; trampolines can optimize mutual recursion

;; try trampoline a few simple Clojure functions
(trampoline list)
(trampoline + 1 2)

;; if the return value is a function, then trampoline assumes you
;; want to call it recursivelly and calls it for you, trampoline
;; manages its own recur, so it will keep calling your function until it
;; stopos returning functions

;; back on tail recursion. take the code of tail-fibo
;; and prepare it for trampolining by wrapping the recursive return case
;; inside a function

;; example only. Don't write code like this
(defn trampoline-fibo [n]
  (let [fib (fn fib [f-2 f-1 current]
              (let [f (+ f-2 f-1)]
                (if (= n current)
                  f
                  #(fib f-1 f (inc current));;returns a function
                  ;; to the trampoline
                  )))]
    (cond
      (= n 0) 0
      (= n 1) 1
      :else (fib 0N 1 2))))
;; call
(trampoline trampoline-fibo 9)

;; we've ported tail-fibo to use trampoline to compare and contrast
;; trampoline and recur.
;; for self-recursions  like trampoline-fibo, trampoline offers no
;; advantage, and you should prefer recur. but with mutual recursions
;; trampoline comes into its own

;; considers the mutually recursive definition of my-odd? and my-even?,
;; which we presented at the beginning of recursion revisited.
;; you can convert these broken, stack consuming implementations to
;; use trampoline, with the same approach you used to convert tail-fibo
;; prepend a # to any recursive tail calls so that they return a function
;; to produce the value, rather tan the value itself.

(declare my-odd? my-even?)
(defn my-odd? [n]
  (if (= n 0)
    false
    #(my-even? (dec n))))
(defn my-even? [n]
  (if (= n 0)
    true
    #(my-odd? (dec n))))

;; # replace recursion with lazyness
;; replace function developed by Eugene Wallingford to demonstrate mutal
;; recursion. replace works with an s-list data structure, which is a list
;; that can contain both symbols and list of symbols

;; fiction example about desigened replace function
(replace '((a b) (((b g r) (f r)) c (d e)) b) 'b 'a)

;;overly-literal port, do not use
(declare replace-symbol replace-symbol-expression)
(defn replace-symbol [coll oldsym newsym]
  (if (empty? coll)
    ()
    (cons (replace-symbol-expression
           (first coll) oldsym newsym)
          (replace-symbol
           (rest coll) oldsym newsym))))
(defn replace-symbol-expression [symbol-expr oldsym newsym]
  (if (symbol? symbol-expr)
    (if (= symbol-expr oldsym)
      newsym
      symbol-expr)
    (replace-symbol symbol-expr oldsym newsym)))
;; the two fnctions replace symbol and replace-symbol-expression are mutually recursive
;; so a deeply nested structure could blow the stack. To demonstrate the problem, create
;; a deelpy nested function that builds deeply nested lists containing a single bottom
;; element
(defn deeply-nested [n]
  (loop [n n
         result '(bottom)]
    (if (= n 0)
      result
      (recur (dec n) (list result)))))
(set! *print-level* 100)
(deeply-nested 5)
(deeply-nested 25)
(deeply-nested 10000)
;; now try to use replace-symbol to change bottom do deepest for different levels of nesting
;; You'll see that large leves blow the stack.
(replace-symbol (deeply-nested 5) 'bottom 'deepest)
(replace-symbol (deeply-nested 10000) 'bottom 'deepest)
;; StackOverflow! all of the recursive calls to replace-symbol are inside a cons. To break the
;; recursion, all you have todo is wrap the recursion with lazy-seq. It's really that simple.
;; Here's the improved version. Since the transition to laziness was so simple, we couldn't
;; resist the temptation to make the function more Clojure-ish in another way as well.
(defn- coll-or-scalar [x & _] (if (coll? x) :collection :scalar))
(defmulti replace-symbol coll-or-scalar)
(defmethod replace-symbol :collection [coll oldsym newsym]
  (lazy-seq
   (when (seq coll)
     (cons (replace-symbol (first coll) oldsym newsym)
           (replace-symbol (rest coll) oldsym newsym)))))
(defmethod replace-symbol :scalar [obj oldsym newsym]
  (if (= obj oldsym) newsym obj))
;; make sure the improved replace-symbol can handle deep nesting
(replace-symbol (deeply-nested 10000) 'bottom 'deepest)
;; why turning into a lazy-seq doens't cause stackoverflow when it is realized?
(source lazy-seq)
;; If you look at the source of lazy-seq, you’ll see it is transforming the code into a “thunk”
;; – a function of no arguments that yields the expression when called – which is invoked when a
;; new element needs to be realized. So the stack never gets deeper than one call, made each time a
;; new element is “requested” (realized) as the lazy sequence is unraveled.
(trampoline + 1 2 3 4)
;;it will keep calling your function until your function stop to return a function

;; #shorcutting recursion with memoization
;; Hofstadter male,female sequences
;; do not use these directly
(declare m f)
(defn m [n]
  (if (zero? n)
    0
    (- n (f (m (dec n))))))
(defn f [n]
  (if (zero? n)
    1
    (- n (m (f (dec n))))))

(time (m 250))
;; memoization trades space for time by caching the results of past calculation
;; when you call a memoized function,  it first checks your input against a map
;; of previous inputs and their outputs. If it finds the input in the map, it can
;; return the output immediately, without having to perform the calculation again

;; rebind m and f to memoized versions of themselves
(def m (memoize m))
(def f (memoize f))
;; now clojure needs to calculate F and M onlyonc for each n
;; the speedup is enourmous
(time (m 250))
;; once te memoization cache is built, "calculation" of cached value is almost instantaneous
(time (m 250))
;; exposin sequences insteads of functions
(def m-seq (map m (iterate inc 0)))
(def f-seq (map f (iterate inc 0)))
;; taking the  nth value from a sequence
(nth m-seq 250)
;; is quite fast, even for larger values
(time (nth m-seq 10000))

;; the case could be a persistent collection not a sequence
;; this example will produce an input sequene (from the range), which stores the values in
;; memor, ten the output of the map, which stores another sequence of values in memory, then
;; the output of the map, which stores another sequence of values, and then the final vector output
(defn square [x] (* x x))
(defn sum-squares-seq [n]
  (vec (map square (range n))))
;; instead, we can perform the intermediate transformation on the input collection
;; values and place the result directly into te output vector, using into with a map transducer
(defn sum-squares
  [n]
  (into [] (map square (range n))))
;; a transducer is a function that captures the essence of a  collection transformation without trying
;; it to the form of either the input collection or teh output collection
;; the into provides the elements from the input range to the map transformation
;; and then adds the resullts directly into the output collection. This happens eagerlyand returns
;; the final vector, avoiding the creation of the intermediate lazy sequences, which are just overhead
;; if we know the output target is a collection

;; ## Optimizing performance
;; we can also compose transducers and apply multiple transformations within into
;; in a single pass over the input

;; imagine you want to find all of the predicate functions in the namespaces
;; we've loaded so far. Predicate fnctions typically end in ?, so we can find all th loaded namespaces
;; the find their public vars, filter down to just those ending in?, and finally convert them to friendly names
;; using sequences we ofter chain transformation together with ->>
(defn preds-seq []
  (->> (all-ns)
       (map ns-publics)
       (mapcat vals)
       (filter #(clojure.string/ends-with? % "?"))
       (map #(str (.-sym %)))))
;; the sequence implementation creates four intermediate sequences. The transducer implementation composes
;; the four transformations into a single combined transformation and applies it during a single transversal of
;; the (all-ns) input sequence

;; how lazyness complicates management of external resources and how eager transformation can help
;; ## Managing external resources
(defn non-blank? [s]
  (not (clojure.string/blank? s)))
(defn non-blank-lines-seq [file-name]
  (let [reader (clojure.java.io/reader file-name)] ;; the problem here is that the reader is not being closed
    (filter non-blank? (line-seq reader))))
;; reader resource is eing left open and stranded in this code. beyound that is not clear when the resource
;; is being closed
;; another approach is to eagerly process all of the lines, then close the resource before returing
;; this solves te dangling resource problem but only by eagerly creating the entire vector of non-blank lines
;; and returning it
(defn non-blank-lines [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (into [] (filter non-blank?) (line-seq reader))))

(defn non-blank-lines-eduction [reader]
  (eduction (filter non-blank?) (line-seq reader)))
;; aneduction is a suspended transformation (like a sequence), but it process the entire input each time its
;; asked
;; for example, we can use non-blank-lines-eduction to count lines
(defn line-count [file-name]
  (with-open [reader (clojure.java.io/reader file-name)]
    (reduce (fn [cnt el] (inc cnt)) 0 (non-blank-lines-eduction reader))))

;;core of clojure: syntax, immutable collection, the sequence abstraction

;; # Clojure Specifications
;; In clojure 1.9 introduced the spec library
;; which allow us to create specs that describe the structure of our data

(require '[clojure.spec.alpha :as s])
;; Specs describingg an ingredient

(s/def ::ingredient (s/keys :req [::name ::quantity ::unit]))
(s/def ::name       string?)
(s/def ::quantity   number?)
(s/def ::unit       keyword?)

;; Function spec fo scale-ingredient
;; these don't just serve as documentation. The spec library uses them to provide several additions
;; tools that operate on spcs - data validation, explanations of invalid data, generation of example data
;; and generative testing for functions
(s/fdef scale-ingredient
  :args (s/cat :ingredient ::ingredient :factor number?)
  :ret ::ingredient)

;; ## Defining specs
;; spec names are qualified keywords
;; when deploying the project artifact
;; namely the qualified part of the keyword should start with a domain name
;; or a product or project name for which you control the tradmark or mindshare in the market
;; for example, :cognitect.transit.handler/name would be sufficient
;; if no qualifier is provided then the current namespace is used as qualifier (::ingredient)

;; validating data
;; any tim you receive a value from a user or an external source, that data may contain
;; values that break your expectations.
;; validations in data values, through data specs
;; valudations for business logic is a separated concern

;; enumerated specs
;; range specs

;; validation into transforming part of system through api protocol
;; how this validations of input values are handled in datomic transaction and datomic query?
;; there are places this is used for validation as config and db
;; Datomic uses specs to define rules about data validations

;; clojure.spec.alpha ;; namespace for clojure spec 
(s/def ::repeat-args
  (s/cat :n (s/? int?) :x any?))
(s/conform ::repeat-args [100 "foo"])
(s/conform ::repeat-args ["foo"])

;; function spec for rand, e can first create an argument spect that is either empty or takes
;; an optional number
(s/def ::rand-args (s/cat :n (s/? number?)))
(s/def ::rand-ret double?)
;; spec for the rand-fn itself
(s/def ::rand-fn
  (fn [{:keys [args ret]}]
    (let [n (or (:n args) 1)]
      (cond (zero? n) (zero? ret)
            (pos? n) (and (>= ret 0) (< ret n))
            (neg? n) (and (<= ret 0) (> ret n))))))
;; we can now tie all these together using s/def
(s/fdef clojure.core/rand
  :args ::rand-args
  :ret  ::rand-ret
  :fn   ::rand-fn)

;; the function opposite accepts a predicate function and creates the opposite predicate function
(defn opposite [pred]
  (comp not pred)) ;; could be made returning a (fn [] (not pred)) but uses comp for bettter semantic
;; the function opposite accepts a predicate function, which we can describe using s/fspec
(s/def ::pred
  (s/fspec :args (s/cat :x any?)
           :ret boolean?))
(s/fdef opposite
  :args (s/cat :pred ::pred)
  :ret ::pred)

;; ## instrumenting functions
;; during development and testing, we can use intrumentation (stest/instrument) to wrap a function
;; with a version that uses spec to verify that the incoming arguments to a function conform to the
;; function's spec
;; !!! instrumentation for args only, the purpose of instrumentation is to verify invocation not implementation

;; once you've defined a spec for a function with s/fdef, call stst/instrment on the fully qualified function
;; symbol to enable it.
(require '[clojure.spec.test.alpha :as stest])
(stest/instrument 'clojure.core/rand)
;; you can alo uses stest/enumerate-namespace to enumerate a collection of all symbols in a namespace
;; to pass to stest/instrument
(stest/instrument (stest/enumerate-namespace 'clojure.core))
;; instrumenting a function replaces its var with a new var that will check args
(rand :boom)

;; ## Generative function testing
;; Classic example-based unit testing relies on the programmer to test a function
;; by writing a series of example inputs, then writing assertions about the return
;; value when the function is invoked with each input
;; In comparison, generative testing is a technique that produces thousands of random inputs,
;; runs a procedure, and verifies a set of properties for ach output.
;; Generative testing is a great technique for getting broader test coverage of your code

;; ## Checking functions
;; Spec implements automated generative testing with the fnctiion check in the namespace
;; clojure.spec.test.alpha (commonly aliased as stest). You can run stest/check on any symbol
;; or symbols that have been specéd with s/fdef

;; @@ All invocationns of spec generators (either directly or within stest/instrument, stest/check, etc.)
;; require including the test.check library as a dependency. Tipically as either a test or dev pofile
;; dependency, so taht it's not included at production runtime

;; Lets see how i works with a spec for the Clojure core function symbol, which takes a name and
;; and an optional namespace
(doc symbol)

;; trainning exercise
;; have a script to get metrics list from a file then write multiple files (indicators)
;; with an edn format to define metrics indicators
;; something to consider, the file is written correctly, hower it could contains blank lines

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(def reader (io/reader "./datomic-indicators"))
(ns-publics 'clojure.java.io)
(with-open [reader (io/reader "./datomic-indicators")])
(line-seq reader)

(map (fn [l] 2)
     (line-seq reader)) ;; after read from reader line seq returns nil
;; reader was closed, better control opening and closing reader with with-open block
;; creating and closing after usage to have it ready for use again

(with-open [r (io/reader "./datomic-indicators")]
  (line-seq r)) ;; it throws an error why? Error printing return value (IOException) at java.io.BufferedReader/ensureOpen (BufferedReader.java:123).
;; Stream closed

;; a lazy sequence is lazy it means it doesn't evaluate the file instead it let it todo later. Then it returns the control
;; exeuction for the rEPL that tries to eval the lazy sequence and resolve the line of files
;; however the file is already closed on that time because it closes right after eval with-open


(with-open [r (io/reader "./datomic-indicators")]
  (doall (line-seq r))) ;; forcing evaluation of all sequence istead do it to evaluate and resolve the files of the lazy sequence

;; example of indicator
#_{
   :name              :datomic-transactor-apply-tx-percentage
   :expr              "max_over_time((sum(datomic_transactor_transaction_apply_msec_sum) by (environment, prototype, service, squad) / 60000) [5m:15s])"
   :frequency-minutes 5
   :result-labels     [:environment :prototype :service :squad]
   :environments      {:prod    #nu/prototypes-for [:prod :sharded+global]
                       :staging #nu/prototypes-for [:staging :sharded+global]}}


nu/prototypes-for
nu/jj

'#nu/prototypes-for


(def j ' )
j
'{:a aaa}

(defn non-blank? [s]
  (not (clojure.string/blank? s)))

(defn non-blank-line
  [line]
  (not (str/blank? line)))

(defn non-blank-line-vec
  [line-vec]
  (not (str/blank? (first line-vec))))

(defn non-header-line
  [line]
  (not (or  (str/includes? line "Metric")
            (str/includes? line "CHANGES"))))

;; it doesn't respect line break
(defn create-indicator-file
  [file-name]
  {:name (keyword file-name)
   :expr (str  "sum(" (str/replace file-name #"-" "_") ") by (environment, prototype, service, squad)")
   :frequency-minutes 60
   :result-labels [:environmnt :prototype :service :squad]
   :environments {:prod [:prod :sharded+global ;; there is problem with the macro #nu/prototypes-for, solve it later
                         :staging [:staging :sharded+global]]}} )
;; it doesn't respect line breaks either
;; it works but the problem is when mapping expression to print using (pr-str)  it prints the object as on line string
;; but it do not respect formatting, if needed specific formatting stuff then needed to instruct computer where are
;; defined the formatting rules. Use string code to define format.
(defn create-indicator-file
  [file-name]
  {:file-name file-name
   :file-content
   (str "{:name " (keyword file-name) "\n"
        " :expr " (str  "sum(" (str/replace file-name #"-" "_") ") by (environment, prototype, service, squad, kubernetes_pod_name)") "\n"
        " :frequency-minutes " 60 "\n"
        " :result-labels " [:environment :prototype :service :squad :kubernetes_pod_name] "\n"
        " :environments {:prod #nu/prototypes-for [:prod :sharded+global] \n"
        "                :staging #nu/prototypes-for [:staging :sharded+global :kubernetes_pod_name]}}\n"
        "")})

(defn split-line
  [line]
  (str/split line #",(?! )" 5))

(create-indicator-file "datomic-apply-msec")

(defn write-indicator-output-file
  [{:keys [file-name file-content]}]
  #_(println (name (:name indicator)))
  (with-open [w (io/writer (str "./indicators/" (name file-name) ".edn"))]
    (.write w file-content)))

;; needs to map to definition in multiple countries
(defn write-indicator-output-file
  [{:keys [file-name file-content]}]
  (let [base-out-path "/Users/filipe.andrade/dev/nu/definition/resources"]
    (with-open [wbr (io/writer (str base-out-path "/br/indicators/" (name file-name) ".edn"))
                wmx (io/writer (str base-out-path "/mx/indicators/" (name file-name) ".edn"))
                wco (io/writer (str base-out-path "/co/indicators/" (name file-name) ".edn"))]
      (.write wbr file-content)
      (.write wmx file-content)
      (.write wco file-content))))


;; gets values from a raw text file format
(with-open [r (io/reader "./datomic-indicators")]
  (into [] (comp (filter non-header-line)
                 (filter non-blank-line)
                 (map create-indicator-file)
                 (map write-indicator-output-file)) (line-seq r))) ;; another approach which takes transducer transformatio;; n in an ouput collection managing external resources

;; optmizing performance with eager transformation direct output for output collection
;; single tranformation we are fine, just need to write the output

;; gets values from a csv file format with information in a sequence


(defn create-indicator-file
  "get a line vector for indicator file and generates the indicator file in expected order
   position 0 -> metric name in snake_case
   position 1 -> indicator name in kbebab-case
   position 2 -> frequency in minutes
   position 3 -> promQL expression"
  [line-vec]
  (let [metric-name    (nth line-vec 0)
        indicator-name (nth line-vec 1)
        frequency      (nth line-vec 2)
        promql-exp     (nth line-vec 3)]
    (println (nth line-vec 3))
    {:file-name indicator-name
     :file-content
     (str "{:name " (keyword indicator-name) "\n"
          " :expr " promql-exp "\n"
          " :frequency-minutes " frequency "\n"
          " :result-labels " [:environment :prototype :service :squad :kubernetes_pod_name] "\n"
          " :environments {:prod #nu/prototypes-for [:prod :sharded+global] \n"
          "                :staging #nu/prototypes-for [:staging :sharded+global]}}\n"
          "")}))

(with-open [r (io/reader "./datomic-txor-indicators.csv")]
  (into [] (comp (filter non-header-line)
                 (map split-line)
                 (filter non-blank-line-vec)
                 (map create-indicator-file)
                 (map write-indicator-output-file)) (line-seq r)))

;; add lib it is available in a version of tools.deps.alpha
;; a functional api for dependency graph expansion the creation of classpaths
;; it runs in add-lib3 branch from tools.deps.alpha

;; invoke command clj command line

clj -Sdeps "{:deps
               {org.clojure/tools.deps.alpha
                {:git/url \"https://github.com/clojure/tools.deps.alpha.git\"
                 :sha \"d492e97259c013ba401c5238842cd3445839d020\"}}}"

clj -Sdeps "{:deps
               {org.clojure/tools.deps.alpha
                {:git/url \"https://github.com/clojure/tools.deps.alpha.git\"
                 :sha \"8f8fc2571e721301b6d52e191129248355cb8c5a\"}}}"

8f8fc2571e721301b6d52e191129248355cb8c5a

;; add correct commit, get latest from add-lib3 branch, restart repl
;; test if add-lib is in classpath
(use 'clojure.tools.deps.alpha.repl)
add-libs
;; it throws exception
;; Unable to resolve symbol: add-lib in this context
;; why?

;; this is not add-lib it is add-libs with S

;; how can I chek the funcions of a namespace to figure out it by myself
(ns-publics 'clojure.tools.deps.alpha.repl)

;; commit d492e97259c013ba401c5238842cd3445839d020 not found
;; https://insideclojure.org/2018/05/04/add-lib/

;; use https://github.com/clojure/tools.deps.alpha/tree/add-lib3 only for add-libs
;; tools.deps is for standard usage

;; including the test.check library
(doc add-libs)

;; example
;;  (add-libs '{org.clojure/core.memoize {:mvn/version "0.7.1"}})

;; https://github.com/clojure/test.check
;; test.check is a Clojure property-based testing tool inspired by QuickCheck
;; instead of enumerating expected input and output for unit tests, you write properties about your function that should hold true for all inputs.

;; adding test check using add-libs
(add-libs '{org.clojure/test.check {:mvn/version "1.1.1"}})

(require '[clojure.test.check :as stest])
(ns-publics 'clojure.test.check)

;; see how it works for symbol clojure
(doc symbol)
;; First we need to define the function spec
(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.test.alpha :as stest2]) ;; generative test for specs
(s/fdef clojure.core/symbol
  :args (s/cat :ns (s/? string?) :name string?)
  :ret symbol?
  :fn (fn [{:keys [args ret]}]
        (and (= (name ret) (:name args))
             (= (namespace ret) (:ns args)))))
;; and then we can run the test as follows
(stest2/check 'clojure.core/symbol)

;; ## Generating arguments
;; the argument we used bfore was for validation
;; we can use s/exercise function which produces pairs of examples and their conformed values
(s/exercise (s/cat :ns (s/? string?) :name string?))
;; sometimes spec can't automatically create a valid generator
;; or need to create a custom generator for related arguments

;; ## Combining generators wit s/and
;; For exmple, consider the following spec for an odd number greater than 100
(defn big? [x] (> x 100))
(s/def ::big-odd (s/and odd? big?))
(s/exercise ::big-odd)
;; add an inntial predicate type oriented predicate
(s/def ::big-odd-int (s/and int? odd? big?))
(s/exercise ::big-odd-int)

;; ## Creating custom generators
;; replace gnerator that only produces red marbles
(s/def :marble/color-red
  (s/with-gen :marble/color #(s/gen #{:red}))) ;; THROWS, try other example
;; generate random sufix
(require '[clojure.string :as str])
(require '[clojure.test.check.generators :as gen])
(s/def ::sku
  (s/with-gen (s/and string? #(str/starts-with? % "SKU-"))
    (fn [] (gen/fmap #(str "SKU-" %) (s/gen string?)))))
;; that generator is then attached to  the spec. lets try exercising it
(s/exercise ::sku)


;; CHALLENGE
;; given a tree of directories, scan for multiple files search for a specific string in the file
;; if the string exists increment a counter and store metadata about the line/file about the occurrence
;; line-seq is only for lines into the file
;; file-seq is for map files in a director
;; reader returns a buffered reader of a file to read a file as a directory needs to use
;; file


(def file (io/file "/Users/filipe.andrade/dev/phil"))
(take 10 (file-seq file))
(.close file)
;; with open call close at the end when exploring file
;; but it java.io.File doesn't implements .close
;; basic answer, we need to differentiate between a File and a FileStream only FileStreams needs to be closed
;; java.io.File doesn't represent an open file, it represents a path in the filesystem.
(let [reader (io/file "/Users/filipe.andrade/dev/datomic")] ;;  reader cannot read a directory
  (into [] (comp
            (map identity)) (file-seq reader)))
;; eager generate sequence, takes time depending on file directory structure
;; but it returns lazy for larger file structures.

(do
  (require '[clojure.java.io :as io]
           '[clojure.string :as str])
  (set! *print-length* 5)
  (set! *print-level* 5))

(str/includes? "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains/common-technology-platforms/src/main/scala/nu/data_domain/common_technology_platforms/CommonTechnologyPlatforms.scala" "data_domain")

(defn filter-data-domain-files
  [file]
  (str/includes? #"data_domain" (.getAbsolutePath file)))

(defn data-domain
  [file]
  (str/includes? (.getAbsolutePath file) "data_domain"))

;; the file seq get the sequence of directories recursivelly
;; print directories recursivelly consume the reader for repl
;; iterates over all data-domains in itaipu
;; and for each file search for prometl sets
(time
 (let [dir (io/file #_"/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"
                       #_"/Users/filipe.andrade/dev/datomic/subtree"
                       "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")] ;;  reader cannot read a directory
   (-> (into [] (comp
                 #_(filter data-domain)) (file-seq dir))
       count)))

(count [1])
;; count for folder is not the same count as linux find -type f
;; find -type f considers only files when getting a java.io.File also getting directories
;; so subfolder as subtree/a/b/c counts as 4 even if it doesn't contain any file
(count (file-seq (java.io.File. "/Users/filipe.andrade/dev/datomic/subtree")))
(count (file-seq (io/file "/Users/filipe.andrade/dev/datomic/subtree"))) ;;same result using io/file or java.io.File, but sometimes is simple to use io library


;; check count of files in domain-data
(count (file-seq (io/file "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")))
;; 36935, files, same count as using
;; find ~/dev/nu/itaipu/subprojects/data-domains | wc -l

;; logic understanding
;; files are not descriptive by pattern name
;; some of files contains prometl string in pattern example 
;; subprojects/data-domains/common-technology-platforms/src/main/scala/nu/data_domain/common_technology_platforms/br/microservices/data_sources/prometl/IndicatorsCtp.scala

;; so for each file search for strings containing prometl
;; count files which has the keyword prometl on it
(time (count (file-seq (io/file "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))))
;; as a eager sequence it is not cached and returns same time duration in each invocation

;; exercise, compare eager vs lazy sequences time execution, to relate this theory
;; seq and coll are differents for example, map generates a seq and vec generates a coll
;; seq = sequence
;; coll = collection
;; example case, we'd need to apply a final vec function to transfer the elements of the sequence
;; back into a collection
(defn square [x] (* x x))
(defn sum-squares-seq [n]
  (vec (map square (range n))))

(time (sum-squares-seq 10000))
(time (sum-squares-seq 1000000))

;; ;; As it executs it is not caching/reducing time of execution what is missing to cache a lazy seq?
;; ;; user=> #'user/sum-squares-seq
;; user=> "Elapsed time: 5.49025 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.285375 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.2035 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.089209 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.028708 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.334792 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 8.197875 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 13.926083 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 10.112583 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 8.067875 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 5.434041 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 6.329125 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 8.52175 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 5.324 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 3.81775 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.994792 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 4.004875 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 3.673667 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 2.971167 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 28.982541 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 46.820292 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 53.678542 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 63.552458 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 54.0955 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 41.290333 msecs"
;; [0 1 4 9 16 ...]
;; ;; CHALLENGE, use emacs mark and region, comment the text block above
;; ;; approach, searching on emacs manual mark and region inside important text changing commands
;; ;; C-M-h move point to the beginning of the current defun
;; ;; problem related to indent commands, and also comment commands related to text editing
;; ;; example as it relates to each other, is C-x TAB innitiate interactive tab for a region
;; ;; example manipulating a region is selecting a region paragraph with M-h and then backspace for deleting
;; ;; there is a whole section about Comments in Programs > Comments > Manipulating comments

;; ohhh hohoho add auto comment to the line M-;
;; when a region is active M-; add the comment to the region

;; figured it out getting back to the clojure question with lazy sequences and cache
(time (sum-squares-seq 1000000))
(def mem-sum-squares-seq
  (memoize sum-squares-seq))
(time (mem-sum-squares-seq 10000000))

;; there we go, memoize the function call for the function that returns the lazy-seq
;; not the lazyseq itself because it would never make free the memory alocated for the head of lazy seq
;; it could cause the memory to be allocated for the all existing of the program

(time (mem-sum-squares-seq 10000000))
;; user=> "Elapsed time: 7.689459 msecs"
;; [0 1 4 9 16 ...]
;; user=> "Elapsed time: 0.091958 msecs" ;; huge differences after elements allocated
;; [0 1 4 9 16 ...]

(set! *print-level* 10)
(set! *print-length* 10)
;; interleave strategy, for each call of dir get file first bytes
(interleave [:a :b] [1 2])
(map identity [:a :b])
(defn walk-data-dir [dir]
  (file-seq (io/file dir)))

(def walk-data-dir-mem (memoize walk-data-dir))

(-> (map identity (walk-data-dir-mem "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))
    count)
;; Throws an error, why?: Don't know how to create ISeq from: clojure.core$memoize$fn__6946
walk-data-dir
;; does it refresh if changes the filesystem?
;; OPPORTUNITY, to exercise, instead to create a file using mkdir use Clojure programmig language
(doc io/file)
(javadoc java.io.File)
;; After read javadoc for File, interesting methods shows up, canExecute, canRead, canWrite
;; these are some methods which makes systems calls to the file system to check file operation
;; so using writeFile do
(.writeFile (io/file "/Users/filipe.andrade/dev/nu/itaiupu/subprojects/data-domains/TEST2"))
;; throws No matching field found: writeFile for class java.io.File
;; because it doesn't write nothing it is just a simple call create new file (empty)
;; remember to write is needed a write buffer
(.createNewFile (io/file "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains/TEST2"))
;; it works, however there are differences between the file created by Java and command line
;; about permissions
;; drwxr-xr-x   2 filipe.andrade  staff    64 Apr  1 14:01 TEST1
;; -rw-r--r--@  1 filipe.andrade  staff     0 Apr  1 14:09 TEST2

;; the result doesn't change, the new file is not included in the list
(-> (map identity (walk-data-dir-mem "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))
    count)
;; keeping the function in a new symbol makes it easy to call to get refreshed contents
(-> (map identity (walk-data-dir "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))
    count)
;; there we go, the count takes more time but it returns 36937, instead 36935
;; time executing map walk files and pmap walk files
(time (-> (map identity (walk-data-dir "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))
    count))
;; "Elapsed time: 920.94075 msecs"
(time (-> (pmap identity (walk-data-dir "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))
    count))
;; "Elapsed time: 989.417167 msecs"

;; Pmap takes more time for simple count files in the filesystem
;; added overhead of thread coordination when execution
;; but when it increase complexity as walk the entire file probably it will pay the price
;; as files has different files, and it will give time of process files in parallel

;; Hypotheses. Time of pmap readind file of a large file hierarchy is lower than sequential map
(defn file-line-count
  [file-path]
  (with-open [reader (io/reader file-path)]
    (-> (line-seq reader) count))) ;; dumb file count

(.write (io/writer "/Users/filipe.andrade/dev/datomic/file") "Hellow!!!!!")
(-> (line-seq (io/reader "/Users/filipe.andrade/dev/datomic/file")) count)

;; opportunity to write a file using program language
;; before, created an empty file now try to write something on it
(.write (io/writer "/Users/filipe.andrade/dev/datomic/file") "Hellow!!!!!")
;; it doesn't work, always is writing an empty file, why?
;; maybe buffer is being closed before write to it?
(with-open [writer (io/writer "/Users/filipe.andrade/dev/datomic/file")]
  (.write writer "Hellow!"));; it works
(-> (io/writer "/Users/filipe.andrade/dev/datomic/file")
    (.write "Hallow!"));; it doesn't work. good. Why?
;; alternative, spit
(spit "/Users/filipe.andrade/dev/datomic/file" "Hallow, brother!");; it works

(.write (clojure.java.io/writer "/tmp/file") "foo") ;; writes empty file
(with-open [wtr (clojure.java.io/writer "/tmp/file")] ;; writes foo as expected
  (.write wtr "foo2"))

;; Flush the buffers!
;; https://www.gnu.org/software/libc/manual/html_node/Flushing-Buffers.html
;; flushing output on a buffered stream means transmitting all accumulated characters to the file

(doto (clojure.java.io/writer "/tmp/file") (.write "fooz") .flush)
;; well the question for it will be answered later, try to get from some coleague from Clojurians
;; https://clojurians.slack.com/archives/C03S1KBA2/p1711998175283429


;; let's try do the trick
;; count line of each file in a directory
;; - first count files in a directory
(-> (map identity (walk-data-dir "/tmp"))
    count)
;; try to sum the count of lines
(-> (map file-line-count (walk-data-dir "/tmp"))) ;; fails
;; the first element includes the directory, so skipping by getting the rest of the sequence
(-> (map identity (rest (walk-data-dir "/tmp")))) ;; now only files
;; count, again
(-> (map file-line-count (rest (walk-data-dir "/tmp"))) ;; cannot support .sock files try other dir
    count)
(-> (map file-line-count (rest (walk-data-dir "/Users/filipe.andrade/dev/datomic/files")))
    count) ;; works, we need to reduce in a sum

(->> (map file-line-count (rest (walk-data-dir "/Users/filipe.andrade/dev/datomic/files")))
     (reduce + 0)) ;; reduce the collection in a sum of all lines

;; works, but introducing a new folder it throws
;;  Execution error (FileNotFoundException) at java.io.FileInputStream/open0 (FileInputStream.java:-2). /Users/filipe.andrade/dev/datomic/files/b (Is a directory)
;; as walk directories this will get a dir instead a file

;; as java produces an exception, catch and igone it
(defn file-line-count
  [file-path]
  (try (with-open [reader (io/reader file-path)]
         (-> (line-seq reader) count))
       (catch java.io.FileNotFoundException e 0) ;;skip file not found
       )) ;; dumb file count
;; try again
(->> (map file-line-count (rest (walk-data-dir "/Users/filipe.andrade/dev/datomic/files")))
     (reduce + 0)) ;; reduce the collection in a sum of all lines

;; lets try the trick for the whole dir of data-domains
;; with more work to-do parallel is benefitial enough even with thread overhead
(time (->> (map file-line-count (rest (walk-data-dir "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")))
           (reduce + 0)))
;;  "Elapsed time: 3600.1745 msecs"

(time (->> (pmap file-line-count (rest (walk-data-dir "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")))
           (reduce + 0)))
;; "Elapsed time: 1394.263458 msecs"

;; now, need to get the lines with prometl and count how much are
;; as the number of operations increases for an output is better to change for an eager approach
;; as we are not taking advantages of lazy sequences

;; base syntax for transducing transformations
(time (into [] (map identity) []))



;; apply transformations, in a composition of functions
(time (into [] (comp (map identity)
                     (map slurp))
            (rest (walk-data-dir "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"))))
;; it fails, same error, but wrapping a function just to catch in exception is too much overhead
;; so instead, introduce filter predicate, functional approah
(set! *print-level* 10)
(set! *print-length* 10)
(require '[clojure.java.io :as io]
         '[clojure.string :as str])
;; read all files, in data_domains and extract all line of code into a collection
(time (-> (into [] (comp (map identity)
                         (filter #(.isFile %))
                         (map (fn [file]
                                (with-open [rdr (io/reader file)]
                                  (vec (line-seq rdr))))) ;; expansion
                         (map (fn [_] true))
                         #_(filter #(not (nil? %)))
                         #_(filter #(str/includes? % "prometl_"))
                         )
                (rest (file-seq (io/file #_"/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"
                                         #_"/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains/identity/src/main/scala/nu/data_domain/identity/br/registry_platform/"
                                         "/Users/filipe.andrade/dev/datomic/files"))))
          ))

;; it returns an array of lines for a file
;; need to apply this filter into the collection of each element
;; filter all lines that contains prometl_
;;27405 files ;; => only 1241 manages prometl (contains prometl) ;; not right yet

;; do in parts
(defn map-files-expand [file-dir]
  (time (-> (into [] (comp (map identity)
                         (filter #(.isFile %))
                         (map (fn [file]
                                (with-open [rdr (io/reader file)]
                                  (vec (line-seq rdr))))))
                (rest (file-seq (io/file file-dir)))))))

#_"/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains"
#_"/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains/identity/src/main/scala/nu/data_domain/identity/br/registry_platform/"
(->> (map-files-expand "/Users/filipe.andrade/dev/datomic/files")
     (filter #(do (str/includes? % "prometl_")
                  #_(println %))))
;; why this is filtering? if only works in the high level
(str/includes? "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAABBBBBBBBBBBBBB" "prometl_")
(str/includes? "AAAAAAAAAAAAAAAAAprometl_BBBBBBBBBBBB" "prometl_")
;; includes accept a vector and if use in a filter returns the entire vector if one of those elements
;; matches the substring
(str/includes? ["AAAAA" "BBBBBB" "CCCCCC" "prometl_"] "prometl_")
(str/includes? ["AAAAA" "BBBBBB" "CCCCCC" "prometl_" 2 3 :four] "prometl_")

(slurp "/tmp/file") ;; it returns all file in a string
;; instead is necessary to get exactly line containing an specific information
(with-open [rdr (io/reader "/tmp/file")]
  (doall (line-seq rdr)))

;; combining sequence functions along the way: reduce, for, count and filter
(for [f (file-seq (io/file "/Users/filipe.andrade/dev/datomic/files"))
      :when (.isFile f)]
  (with-open [rdr (io/reader f)]
    (count (filter #(str/includes? % "prometl_") (line-seq rdr))))
  )

;; creating a new function
(defn count-prometl-files [file-dir]
  (for [f (file-seq (io/file file-dir))
        :when (.isFile f)]
    (with-open [rdr (io/reader f)]
      (count (filter #(str/includes? % "prometl_") (line-seq rdr))))))
(count-prometl-files "/Users/filipe.andrade/dev/datomic/files")
;; return prometl-files-matches
(defn prometl-files [file-dir]
  (for [f (file-seq (io/file file-dir))
        :when (and (.isFile f)
                   (not (.isHidden f)))]
    (with-open [rdr (io/reader f)]
      (when-let [lines (vec (filter #(str/includes? % "Attribute(\"prometl_") (line-seq rdr)))]
        [(.getAbsolutePath f) lines]))))

(set! *print-level* 10)
(set! *print-length* 10)
(require '[clojure.java.io :as io]
         '[clojure.string :as str])

;; small scale
(let [r (->> (prometl-files "/Users/filipe.andrade/dev/datomic/files")
             (filter #(not (empty? (second %)))))]
  (for [rr r]
    (with-open [wtr (io/writer "/Users/filipe.andrade/dev/datomic/results" :append true)]
      (.write wtr (str (first rr) "," (second rr))))))

;; large scale
(let [r (->> (prometl-files "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")
             (filter #(not (empty? (second %)))))]
  (for [rr r]
    (with-open [wtr (io/writer "/Users/filipe.andrade/dev/datomic/prometl-attributes.csv" :append true)]
      (.write wtr (str (first rr) "," (second rr))))))

(defn prometl-files [file-dir]
  (for [f (file-seq (io/file file-dir))
        :when (and (.isFile f)
                   (not (.isHidden f)))]
    (with-open [rdr (io/reader f)]
      (when-let [lines (vec (filter #(str/includes? % "prometl_") (line-seq rdr)))]
        [(.getAbsolutePath f) lines]))))

;; same version for large and small scale, trying to generate a csv readable file
(defn prometl-attributes
  [prometl-files]
  (let [rows (filter #(not (empty? (second %))) prometl-files)]
    (for [row rows]
      (with-open [wtr (io/writer "/Users/filipe.andrade/dev/datomic/prometl-attributes.csv" :append true)]
        (doall (for [att (second row)]
                 (.write wtr (str (first row) "," (str/trim att) "\n"))))
        true))))

;; reads prometl files expand and write output in a csv readable format, small scale
(-> (prometl-files "/Users/filipe.andrade/dev/datomic/files")
    (prometl-attributes))

;; large scale
(-> (prometl-files "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")
    (prometl-attributes))

#_(prometl-files "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")

#_(->> (prometl-files "/Users/filipe.andrade/dev/nu/itaipu/subprojects/data-domains")
       (filter #(not (empty? (second %)))))

;; # STATE AND CONCURRENCY
;; # Chapter 6

;; A state is the value of an identity at a point in time;
;; A value is an immutable, persistent data structure. When you can program
;; entirely with valus, life is easy, as we saw in Chapter 4, functional Programming

;; The flow of time makes things substantially more difficult.

;; Clojure's reference model clearly separates identities from values. Almost everything in Clojure
;; is a value. For identites, Clojure provides four reference types:

;; Refs manage coordinated, synchronos chages to shared state
;; Atoms managge uncoordinated, synchornous changes to shared state
;; Agents manage asynchronous changes to shared state
;; Vars manage thread-local state


;; Clojure identities apis. values and identities are separated
;; refs, atoms, agents, vars

;; a change in the value, should not impact the ability to refer to the identity

;; operations that are logically independent are easier to implement in parallel
;; cas operations changing state

(def current-track (ref "Mars, the Bringer of War"))
(deref current-track)
;; using shortened macro @
@current-track

;; requires a lock to the ref, dosync does that
(dosync (ref-set current-track "Venus, the Bringer of Peace"))

;; software transactional memory (STM)
;; Databases provide the additional guarantee that updates are durable
;; Clojure's STM provides ACI

;; Coordinating multiple updates
(def current-track (ref "Venus, the Bringer of Peace"))
(def current-composer (ref "Holst"))

(dosync
 (ref-set current-track "Credo")
 (ref-set current-composer "Byrd"))

;; chat example
(defrecord Message [sender text])
(->Message "Aarno" "Hello")

(def messages (ref ()))

;; naive add
(defn naive-add-message
  [msg]
  (dosync (ref-set messages (cons msg @messages))))

;; cas operation, read and update in the same operation
(defn add-message [msg]
  (dosync (alter messages conj msg)))

;; adding messages
(add-message (->Message "user 1" "hello"))

;; MVCC, used by STM multiversion concurrency control
;; communiting
(defn add-message-commute [msg]
  (dosync (commute messages conj msg)))

;;prefer alter
;; using a increasing sequence count of numbers to build uuids
(def counter (ref 0))
(defn next-counter [] (dosync (alter counter inc)))
(next-counter)

;;validation function
(defn valid-message? [msg]
  (and (:sender msg) (:text msg)))

(def validate-message-list #(every? valid-message? %))
(def messages (ref () :validator validate-message-list))

(add-message "not a valid message")
(add-message (->Message "Aaron" "Real Message"));; match constraint, no problem
;; Refs are great for coordinated access to shared state
;; for updating a single piece of isolated data, prefer an atom
;; atoms are a lighter-weight mechanism than refs

;; coordination of multiple updates, use refs
;; atom
(def current-track (atom "Venus, the Bringer o Peace"))
@current-track
(reset! current-track "Credo")
;; update multiple values same data structure
(def current-track (atom {:title "Credo" :composer "Byrd"}))
(swap! current-track assoc :title "Sante Deus")

;; if it tolerates updates happenig async, prefer agent
;; use agents for asynchronous updates
(def counter (agent 0))
(send counter inc)

(await (send counter inc))

;; ensure it is a number
(def counter (agent 0 :validator number?))
(send counter (fn [_] "boo"))

(agent-error counter)
;; restart agent to make it active after error
(restart-agent counter 0)
@counter

;;handling agent errors
(defn handler [aget err]
  (println "ERR!" (.getMessage err)))
(def counter2 (agent 0 :validator number? :error-handler handler))
(send counter2 (fn [_] "boo"))
(send counter2 inc)
@counter2

;; using agents to side-effects in transactions
(def backup-agent (agent "./output/messages-backup.clj"))
(defn add-message-with-backup [msg]
  (dosync
   (let [snapshot (commute messages conj msg)]
    (send-off backup-agent (fn [filename]
                            (spit filename snapshot)
                            filename))
    snapshot)))
(add-message-with-backup (->Message "John" "Message One"))

;;vars
(def ^:dynamic foo 10)
(.start (Thread. (fn [] (println foo))))
;; local binding
(defn print-foo [] (println foo))
(binding [foo "boud foo"] (print-foo))

(defn ^:dynamic slow-double [n]
  (Thread/sleep 100)
  (* n 2))

(defn calls-slow-double []
  (map slow-double [1 2 1 2 1 2]))

(time (dorun (calls-slow-double)))


;; The snake game
(ns reader.snake
  (:import (java.awt Color Dimension)
           (javax.swing JPanel JFrame Timer JOptionPane)
           (java.awt.event ActionListener KeyListener))
  (:refer examples.import-static :refer :all))
(import-static java.awt.event.KeyEvent VK_LEFT VK_RIGHT VK_UP VK_DOWN)


(def width 75)
(def height 50)
(def point-size 10)
(def turn-millis 75)
(def win-length 5)
(def dirs {VK_LEFT [-1 0]
           VK_RIGHT [1 0]
           VK_UP [0 -1]
           VK_DOWN [0 1]})


;; Midi
(import '[javax.sound.midi MidiSystem])

(defprotocol MidiNote
  (to-msec [this tempo])
  (key-number [this])
  (play [this tempo midi-channel]))

(defn perform [notes & {:keys [tempo] :or {tempo 88}}]
  (with-open [synth (doto (MidiSystem/getSynthesizer).open)]
    (let [channel (aget (.getChannels synth) 0)]
      (doseq [note notes]
        (play note tempo channel)))))

(defrecord Note [pitch octave duration]
  MidiNote
  (to-msec [this tempo]
    (let [duration-to-bpm {1 240, 1/2 120, 1/4 60, 1/8 30, 1/16 15}]
      (* 1000 (/ (duration-to-bpm (:duration this))
                 tempo))))
  (key-number [this]
    (let [scale {:C 0, :C# 1, :Db 1, :D 2,
                 :D# 3, :Eb 3, :E 4, :F 5,
                 :F# 6, :Gb 6, :G 7, :G# 8,
                 :Ab 8, :A 9, :A# 10, :Bb 10,
                 :B 11}]
       (+ (* 12 (inc (:octave this)))
          (scale (:pitch this)))))
  (play [this tempo midi-channel]
    (let [velocity (or (:velocity this) 64)]
      (.noteOn midi-channel (key-number this) velocity)
      (Thread/sleep (to-msec this tempo)))))

;; static
(def close-encounters [(->Note :D 3 1/2)
                       (->Note :E 3 1/2)
                       (->Note :C 3 1/2)
                       (->Note :C 2 1/2)
                       (->Note :G 2 1/2)])
close-encounters

;; dynamic
(def jaws (for [duration [1/2 1/2 1/4 1/4 1/8 1/8 1/8 1/8]
                pitch [:E :F]]
            (Note. pitch 2 duration)))

(perform close-encounters)
(perform jaws)

;; Expanding macros
(defmacro unless [expr form]
  (list 'if expr nil form))
(macroexpand-1 '(unless false (+ 1 2)))


(defmacro chain
  ([x form] `(. ~x ~form))
  ([x form & more] `(chain (. ~x ~form) ~@more)))

(macroexpand '(chain arm getHand getFinger))

;; new macro bench
;; (bench (str "a" "b"))
;; should expand to
(let [start (System/nanoTime)
      result (str "a" "b")]
  {:result result :elapsed (- (System/nanoTime) start)})

;; This won't work
(defmacro bench [expr]
  `(let [start (System/nanoTime)
         result ~expr]
     {:result result :elapsed (- (System/nanoTime) start)}))

(bench (str "a" "b"))
`foo# ;; auto-gensym


(defmacro bench [expr]
  `(let [start# (System/nanoTime)
         result# ~expr]
     {:result result# :elapsed (- (System/nanoTime) start#)}))

(bench (str "a" "b"))
(bench (+ 1 2))
;; inspector
(require '[clojure.inspector :refer [inspect inspect-tree]])
(inspect-tree (System/getProperties))
(inspect-tree {:clojure {:creator "Rich" :runs-on-jvm true}})

;; clojure test
(require '[clojure.test :refer [is]])
(is (string? 10))
(is (string? "hello!"))

;; pinger
(import '[java.net URL HttpURLConnection])
(defn response-code [address]
  (let [conn ^HttpURLConnection (.openConnection (URL. address))
        code (.getResponseCode conn)]
    (when (< code 400)
      (-> conn .getInputStream .close))
   code))
(response-code "http://google.com")

;; BITWISE operations
(comment
  (def rng (java.security.SecureRandom.)) ;; strong random number generator
  (spool nil [1 2 3] "abcde")

  (str (bit-and 111111111111111111111111 0xFFFFF))
  
  (write-chunks nil [[{:key "index" :rev 0 :body "a"} {:key "index" :rev 0 :body "a"}]])

  (doc bit-and)
  (doc byte-array)
  (doc frequencies)

  (frequencies [1 2 3 4 1 3])

  ;; todo ddb-call
  ;; todo map additional keys
  ;; todo write ddb conditional put

  ;; todo generate the nonce
  ;; validate hypothesis from ghadi, probably better - fixed 8 byte string nonce

  ;; secure random is not a question;
  ;; it generates, random bytes

  ;; question is, we have to transform this in string.

  ;; the random bits have to be encoded, to string
  ;; because the nonce should be on range-keys

  (Long/toBinaryString 100000000000000)


  (bit-and 4295543046813048377 0xF)

 
  0xF

  (Long/toBinaryString 255) ;; word 8-bits
  (Long/toBinaryString 15) ;; nyble 4-bits
  
  ;; 

  ;; 8-bit
  ;; 00000000
  ;; 11111111

  ;; 48-bit
  ;; 00000000
  ;; 00000000
  ;; 00000000
  ;; 00000000
  ;; 00000000
  ;; 00000000

  
  ;; 111111111111111111111111111111111111111111111111

  ;; negative point, allocating wasted memory in the heap
  ;; generating the bits specifically 

  ;; important note about the approach of go vs threads
  ;; on thread model
  ;; illustrates the parts will not be re-created ever execution
  ;; of call to the api. https://www.programcreek.com/2013/04/jvm-run-time-data-areas/

  ;; a lot of stuff going on
  ;; https://kamilachyla.com/en/posts/jvm-spec/chapter_2_instruction_set/
  ;; do not have to create all the areas in memory to operate in the
  ;; functions, of the heavy thread

  ;; Long, (str some-48-bit-int)

  ;; Longs are 64-bit
  ;; and we need 48-bit
  ;; so we generate 64-bit Long and wast time 26 bits, each key generation

  (count (str (.nextLong ^java.security.SecureRandom rng)))


  (frequencies (map count
                    (repeatedly 500000
                                (fn [] (str (.nextLong ^java.security.SecureRandom rng))))))
  
  ;; todo put byte-buffer

  ;; understanding the bit-and op;
  ;; bitwise operations work in individual bits
  ;; if want to work with binary numbers, and performe operations
  ;; on then have to use them

  ;; programmer technique to balance the space


  ;; 48 bit hex
  0xFFFFFFFFFFFF

  ;; decimal
  281474976710655
  8921284676249014688 ;; 64 bit long

  ;; decimal result after bitwise AND
  272393441036266 ;; a 48 bit long

  ;; generating hex string
  0xf7bd8a4ac7ea
  (Long/toString 0xf7bd8a4ac7ea 2)
  ;; generated binary
  (count "111101111011110110001010010010101100011111101010")


  (def hex (java.util.HexFormat/of))
  ;; seq in bytes return bytes in hex
  (random-uuid)

  (map #(.toHexDigits hex %) (.getBytes "ffafdea8-774d-4cd7-acc4-fc1e74303492" "UTF-8"))
  (map #(.toHexDigits hex %) (.getBytes "281474976710655" "UTF-8"))
  (map #(.toHexDigits hex %) (.getBytes "f7bd8a4ac7ea" "UTF-8"))
  (map #(.toHexDigits hex %) (.getBytes "SHxur977" "UTF-8"))
  ;; user=> ("66" "66" "61" "66" "64" "65" "61" "38" "2d" "37" "37" "34" "64" "2d" "34" "63" "64" "37" "2d" "61" "63" "63" "34" "2d" "66" "63" "31" "65" "37" "34" "33" "30" "33" "34" "39" "32") - 288 bits < 256 bit string
  ;; user=> ("32" "38" "31" "34" "37" "34" "39" "37" "36" "37" "31" "30" "36" "35" "35") - 120 bits < 48 bit long
  ;; user=> ("66" "37" "62" "64" "38" "61" "34" "61" "63" "37" "65" "61") - 96 bits < 48 bit hex
  ;; user=> ("53" "48" "78" "75" "72" "39" "37" "37") - 64 bits < 48 bit base64


  ;; uuid, long, hex, base64.

  (count '("66" "66" "61" "66" "64" "65" "61" "38" "2d" "37" "37" "34" "64" "2d" "34" "63" "64" "37" "2d" "61" "63" "63" "34" "2d" "66" "63" "31" "65" "37" "34" "33" "30" "33" "34" "39" "32"))

  ;; base64 vs hex str  ~=33%
  ;; base64 vs long str ~=~46%


  ;; binary AND
  ;; 111101111001110110001010010010101100011111101010101000110100000
  ;; 111111111111111111111111111111111111111111111111

  ;; result
  ;; 111101111011110110001010010010101100011111101010

  (Long/parseUnsignedLong "111101111011110110001010010010101100011111101010", 2)


  (Long/toString 272393441036266, 16)

  ;;ASCII or UTF-8 -> this what means str, encoding into a String
  ;; In Java a String represents a string in the UTF-16
  ;; The "platform's default charset" is what your locale variables say it is. That is, UTF-8.

  
  

  (Long/toBinaryString 281474976710655)
  (Long/toBinaryString 8921284676249014688)

  new BigInteger(s, 16).toString(2);


)
;; what is the characteristics of map, and mapv?
(require '[clojure.repl :refer :all])
(doc map)
(doc mapv)
(source map)
;; map
;; returns a lazy seq
;; can accept multiple colls
;; if not coll returns a transducer

;; mapv is eager
;; returns is always a vec

;;; ############### Study case of byte buffer manipulation

;; why not use a transducer for the map?
;; and do it in O^n instead O^n+2

;; what a asymptotic observation says about a function?
;; it tells you how fast a function grows or
;; declines.

;; why call remaining?
(defn concat-bbufs
  "Returns concatenation of bbufs into single ByteBuffer. Will not
  advance the position of bbufs."
  ^ByteBuffer [bbufs]
  (let [bufs (mapv duplicate bbufs)
        len (transduce (map remaining) + bufs)
        bb (ByteBuffer/allocate len)]
    (run! #(.put bb ^ByteBuffer %) bufs)
    (.flip bb)))


(use 'clojure.java.javadoc)
(javadoc javax.sound.midi.MidiSystem)
(require '[clojure.repl :refer :all])
(doc aget)
(doc await)
(doc conj) ;; conjoin
(doc cons) ;; return a new sequence
(doc doall)
(doc for)
(doc str/includes?)
(doc pmap) ;; cannot be used as transducer
java.io.FileNotFoundException
(javadoc java.io.File)
(doc vec)
(doc reduce)
(ns-publics 'clojure.java.io)
(doc io/writer)
(doc io/reader)
(doc str/includes?)
(doc rest)
(doc into)
(javadoc java.io.BufferedWritter)
java.io.BufferedWritter

;; testing java interop
(String. "Fil")
(.getBytes (String. "Fil"))
(java.io.File. "/Users/filipe.andrade/dev")
file
;; try to get absolut path
;; check paths available
(javadoc java.io.File)

;; test clojure add-lib
(doc pmap)
(doc interleave)
(doc memoize)
(doc vec)
(doc reduce)
(doc count)
(doc comp)
(doc str/includes?)
javadoc
(javadoc java.io.File)
(javadoc Thread)
(javadoc String)
(ns-publics 'clojure.java.io)
str/includes?
(doc io/file)
(doc file-seq)
(doc into)
(doc s/cat) ;; returns a regex of all matches
(ns-publics 'clojure.string)
(doc count)
(doc nth)
(doc str/includes?)
(doc str/join)
(doc str/split)
(doc pr-str)
(doc filter)
(require '[clojure.repl :refer :all])
(doc str/blank?)
(doc name)
(doc io/writer)
(doc str/replace)
(doc into)
(doc map)
(doc line-seq)
(doc clojure.java.io/reader)
(doc with-open)
(- 1 0)
(doc rand) ;; returns a random floating number between 0 and n
(doc s/cat)
(doc repeat)
(doc eduction)
(doc into)
(doc map)
(doc vec)
(doc replace)
(doc dec)
(doc curry)
(doc bit-and)
(doc partial)
(doc if)
(doc =)
(doc set!)
(doc take)
(doc letfn)
(doc lazy-seq)
(doc cons)
(doc rem)
(doc iterate-=)
(doc comp) ;; apply the right to the args and the left to the values
;; the rightest to the args and left-over to the values in sequence
(doc count-if)


(source bit-and)
(source partial)
(source letfn)
(source comp)

(tail-fibo 9)
(tail-fibo 10000000)
true

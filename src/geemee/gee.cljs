(ns geemee.gee
  (:require [gamma.api   :as g]
            [geemee.api  :as a]
            [clojure.zip :as zip]
            [clojure.set :as set]))

;; ======================================================================
;; defonce?
(def MAX-RANDOM-CODE-DEPTH  6);; 10)     ;; emperically got to this...
(def MAX-GOOD-CODE-ATTEMPTS 200)   ;; don't want to give up too quickly
(def PROB-TERM-FN           0.5);;0.1)   ;; probability of term-fn vs term-vals
(def PROB-TERNARY-FN        0.2);;0.02)  ;; vs. binary or unary
(def PROB-BINARY-FN         0.5);;0.3)   ;; vs ternary or unary
(def PROB-SINGLE-MUTATION   0.95)  ;; mostly mutate vs. copy

;; ======================================================================
;; Functions used in creating imagery.
(declare random-value)
(defn- random-scalar [] (random-value))
(defn- random-vec2 [] (list 'g/vec2 (random-value) (random-value)))
(defn- random-vec3 [] (list 'g/vec3 (random-value) (random-value) (random-value)))
(defn- random-vec4 [] (list 'g/vec4 (random-value) (random-value) (random-value) (random-value)))

(defn pos3a [] (list 'g/vec3 'pos (random-value)))
(defn pos3b [] (list 'g/vec3 (random-value) 'pos ))

(def term-vals #{'pos random-scalar random-vec2 random-vec3 random-vec4})

(def term-fns #{pos3a pos3b})

(def unary-fns #{'g/radians 'g/degrees 'g/sin 'g/cos 'g/tan 'g/asin 'g/acos 'g/atan
                 'g/exp 'g/log 'g/exp2 'g/log2 'g/sqrt 'g/inversesqrt
                 'g/abs 'g/sign 'g/floor 'g/ceil 'g/fract 'g/normalize
                 ;; these return 1 value
                 ;;'g/length
                 })
(def binary-fns #{'g/+ 'g/* 'g/- 'g/div
                  ;; these don't work
                  'g/atan 'g/pow 'g/mod 'g/step
                  ;; 'g/max 'g/min (wrong args vec3?)
                  ;;'g/distance 'g/dot 'g/cross 'g/reflect
                  })
(def ternary-fns #{'g/smoothstep 'g/mix
                   ;;'g/clamp 'g/faceforward 'g/refract
                   })
(def fns (set/union unary-fns binary-fns ternary-fns))

;; ======================================================================
(defn- random-fn
  "return a random function.  Parameter n selects either 1 or 2
  parameters."
  [num-function-parameters]
  (case num-function-parameters
    3 (rand-nth (seq ternary-fns))
    2 (rand-nth (seq binary-fns))
    1 (rand-nth (seq unary-fns))))

;;(random-fn 3)

(defn- random-value
  "return a random value in the range (-3,3) with only 4 significant
  digits to increase readability"
  []
  (let [x (* 3 (dec (rand 2)))
        x (/ (Math/floor (* x 10000)) 10000.0)]
    x))

(defn- random-terminal
  "return a random terminal value: vectors, position, or noise."
  [out-width]
  (if (< (rand) PROB-TERM-FN)
    ((rand-nth (seq term-fns))) ;; FIXME for out-width
    (case out-width
      4 (random-vec4)
      3 (random-vec3)
      2 (random-vec2)
      1 (random-scalar))))
;;    (let [x (rand-nth (seq term-vals))]
;;      (if (not= x 'pos) (x) x))))
;;(random-terminal)

(defn- random-code
  "Recursively create & return a random s-expression made up of
  functions or terminals. When depth=0, create a terminal to control
  the size.  Create terminal fn with increasing probability as depth
  gets smaller.  Functions are not parameter-checked so runtime
  exceptions can be expected."
  ([out-width depth]
   (if (and (pos? depth) (pos? (rand-int depth)))
     (if (< (rand) PROB-TERNARY-FN)
       (cons (random-fn 3) (repeatedly 3 #(random-code out-width (dec depth))))
       (if (< (rand) PROB-BINARY-FN)
         (cons (random-fn 2) (repeatedly 2 #(random-code out-width (dec depth))))
         (cons (random-fn 1) (repeatedly 1 #(random-code out-width (dec depth))))))
     (random-terminal out-width))))
;;(random-code 5)

(defn- locs
  "return all zip locations within the s-expression.  each location
  contains the full context within the tree for use in replacement
  later."
  [G]
  (let [zipper (zip/seq-zip G)
        all-locs (take-while (complement zip/end?) (iterate zip/next zipper))]
    all-locs))

(defn- locs-ex-fns
  "return all zip locations within the s-expression--but not
  functions, only s-expr and the operands.  each location contains the
  full context within the tree for use in replacement later."
  [G]
  (filter #(not (fns (zip/node %))) (locs G)))

(defn- replace-loc
  "replace the location loc1 with the location loc2, returning the
  root (full s-expression) of loc1."
  [loc1 loc2]
  (zip/root (zip/replace loc1 (zip/node loc2))))

(defn- replace-loc-with-node
  "replace the location loc1 with the location loc2, returning the
  root (full s-expression) of loc1."
  [loc1 node2]
  (zip/root (zip/replace loc1 node2)))

(defn- breed
  "find a random expression in L to replace with a random expression
  in R.  replace it within L and return a new L s-expression."
  [L R]
  (let [loc1 (rand-nth (locs-ex-fns L))
        loc2 (rand-nth (locs-ex-fns R))]
    ;;(println "\nLOC1" loc1)
    ;;(println "\nLOC2" loc2)
    (replace-loc loc1 loc2)))

(defn- mutate-symbol
  "helper function for mutate-node.  Mutates symbols according to Karl
  Sims' SIGGRAPH paper."
  [node]
  (if (term-vals node)
    ;; must be pos--offset it
    (cons `v+ (cons (random-vec2) 'pos))
    (if (term-fns node)
      (rand-nth (seq term-fns))
      (if (unary-fns node)
        (rand-nth (seq unary-fns))
        (if (binary-fns node)
          (rand-nth (seq binary-fns))
          (if (ternary-fns node)
            (rand-nth (seq ternary-fns))
            (println "UNEXPECTED NODE:" node)))))))

(defn- mutate-node
  "Mutates code nodes according to Karl Sims' SIGGRAPH paper.  Returns
  nil sometimes to allow for copying other nodes in calling fn"
  [node]
  (when (< (rand) PROB-SINGLE-MUTATION) ;; mostly mutate here, return nil 5%
    (cond
      ;;* If the node is a scalar value, it can be adjusted by the
      ;;addition of some random amount.
      (number? node)
      (+ node (random-value))
      ;;* If the node is a vector, it can be adjusted by adding random
      ;;amounts to each element.
      (vector? node)
      (vec (map #(+ % (random-value)) node))
      ;;* If the node is a function, it can mutate into a different
      ;;function. For example (abs X) might become (cos X). If this
      ;;mutation occurs, the arguments of the function are also adjusted
      ;;if necessary to the correct number and types.
      ;;[I will keep to same function type]
      (fn? node)
      (mutate-symbol node)
      (symbol? node)
      (mutate-symbol node)
      (list? node)
      (if (< (rand) 0.5)
        ;; variation on above
        (cons (mutate-symbol (first node)) (rest node))
        ;;* An argument to a function can jump out and become the new value
        ;;for that node. For example (* X .3) might become X. This is the
        ;;inverse of the previous [next] type of mutation.
        (rand-nth (rest node)))
      :default
      (println "UNEXPECTED TYPE:" node (type node))
      )))
;;[See mutate fn]* Finally, a node can become a copy of another node from the
;;parent expression. For example (+ (abs X) (* Y .6)) might
;;become (+ (abs (* Y .6)) (* Y .6)). This causes effects similar to
;;those caused by mating an expression with itself. It allows for
;;sub-expressions to duplicate themselves within the overall
;;expression.

;;[TBD]* An expression can become the argument to a new random
;;function. Other arguments are generated at random if
;;necessary. For example X might become (* X .3).

(defn- mutate
  "mutate the code string L according to Karl Sims' SIGGRAPH paper."
  [L]
  (let [loc1      (rand-nth (locs L))
        loc2      (rand-nth (locs-ex-fns L))
        loc3      (rand-nth (locs-ex-fns L))
        new-node  (mutate-node (zip/node loc1))
        ;;_ (println "newnode" new-node)
        ]
    (if (nil? new-node)
      ;; copy random nodes & sub-nodes
      (replace-loc loc2 loc3)
      ;; or, replace with new mutant
      (replace-loc-with-node loc1 new-node))))

(defn- good-random-code?
  "does code x have at least one paren?"
  [x]
  (= (first (pr-str x)) \( ))

(defn- third [x] (nth x 2)) ;; should be stdlib

;;

(defn- get-good-code*
  "The main code-generation workhorse function.  Given a
  code-creator-fn (random, breed or combine), return some code that
  creates non-boring images.  Tries for a while, but if it gives up,
  it returns nil."
  [code-creator-fn]
  (try
    (let [cur-count  (atom 0)
          good-image (atom false)
          ;;[old-hashes old-image-hashes] (get-old-hashes)
          good-code  (atom nil)]
      (while (and (< @cur-count MAX-GOOD-CODE-ATTEMPTS)
                  (not @good-image))
        (swap! cur-count inc)
        (try
          (let [cur-code (code-creator-fn)
                ;;_ (println "\n??" cur-code)
                ;;_ (when-not (nil? (old-hashes (hash cur-code)))
                ;;    (throw (js/Error. "previously created code")))
                _ (when-not (good-random-code? cur-code)
                    (throw (js/Error. "badly created code")))
                ;; FIXME -- have to try out drawing
                ;;img (image (eval cur-code) :size TEST-IMAGE-SIZE)
                ;;_ (when-not (nil? (old-image-hashes (image-hash img)))
                ;;    (throw (js/Error. "previously created image")))
                ;; FIXME? _ (when-not (good-image? img)
                ;;    (throw (js/Error. "boring image")))
                ]
            ;; no exception--got a good one
            (println "\n" @cur-count "got:" cur-code)
            (reset! good-image true)
            (reset! good-code cur-code))
          (catch js/Error e
            ;;(println @cur-count "js/Error" (.getMessage e))
            (print "e")
            )
          ;;(catch java.util.concurrent.ExecutionException e
          ;;  ;;(println @cur-count "execution exception")
          ;;  (print "E")
          ;;  )
          ))
      @good-code)
    (catch js/Error e
      (println "get-good-code* Setup js/Error" e)
      nil)))

(defn- get-random-code
  "get a good image-creation code created randomly"
  []
  ;;(get-good-code* (fn [] (random-code MAX-RANDOM-CODE-DEPTH)))
  (list 'defn 'pixel '[pos] (random-code 3 MAX-RANDOM-CODE-DEPTH)))

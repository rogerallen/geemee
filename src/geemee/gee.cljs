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
(defn pos1a [] (list 'g/swizzle 'pos ':x))
(defn pos1b [] (list 'g/swizzle 'pos ':y))

(def term-vals #{'pos random-scalar random-vec2 random-vec3 random-vec4})

(def term3-fns #{pos3a pos3b}) ;; FIXME eventually noise, etc
(def term2-fns #{'pos})
(def term1-fns #{pos1a pos1b})

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

(defn- random-term-fn
  "return a random terminal function with a specific width"
  [out-width]
  (case out-width
    3 ((rand-nth (seq term3-fns)))
    2 'pos ;;((rand-nth (seq term2-fns)))
    1 ((rand-nth (seq term1-fns)))))

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
    (random-term-fn out-width)
    (case out-width
      3 (random-vec3)
      2 (random-vec2)
      1 (random-scalar))))
;;    (let [x (rand-nth (seq term-vals))]
;;      (if (not= x 'pos) (x) x))))
;;(random-terminal)

(declare random-code1)
(defn- random-code
  "Recursively create & return a random s-expression made up of
  functions or terminals. When depth=0, create a terminal to control
  the size.  Create terminal fn with increasing probability as depth
  gets smaller.  Functions are not parameter-checked so runtime
  exceptions can be expected."
  ([out-width depth]
   (if (and (pos? depth) (pos? (rand-int depth)))
     (if (< (rand) PROB-TERNARY-FN)
       (cons (random-fn 3) (repeatedly 3 #(random-code1 out-width (dec depth))))
       (if (< (rand) PROB-BINARY-FN)
         (cons (random-fn 2) (repeatedly 2 #(random-code1 out-width (dec depth))))
         (cons (random-fn 1) (repeatedly 1 #(random-code1 out-width (dec depth))))))
     (random-terminal out-width))))

(defn- random-code1
  "make various independent formulas, depending on out-width"
  [out-width depth]
  (let [r (rand)]
    (case out-width
      3 (cond
          (< r 0.25) (list 'g/vec3
                           (random-code 1 depth)
                           (random-code 1 depth)
                           (random-code 1 depth))
          (< r 0.50) (list 'g/vec3
                           (random-code 2 depth) (random-code 1 depth))
          (< r 0.75) (list 'g/vec3
                           (random-code 1 depth) (random-code 2 depth))
          :else      (random-code 3 depth))
      2 (cond
          (< r 0.5) (list 'g/vec2
                          (random-code 1 depth) (random-code 1 depth))
          :else      (random-code 2 depth))
      1 (random-code 1 depth)
      :else nil))) ;; FIXME ERROR

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
    (if (term3-fns node) ;; FIXME term1, term2, etc.
      (rand-nth (seq term3-fns))
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

(defn- get-random-code
  "return a function that creates random gamma shader RGB code"
  []
  (random-code1 3 MAX-RANDOM-CODE-DEPTH))

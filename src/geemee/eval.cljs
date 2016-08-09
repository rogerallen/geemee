(ns geemee.eval
  (:require [cljs.js :as cljs])
  (:require-macros [geemee.macros :as macro]))

;; This file's code was initially brought over from klangmeister.
;; So, I'm including the required license.
;;
;; https://github.com/ctford/klangmeister/blob/master/src/klangmeister/compile/eval.cljs
;;
;; Copyright (c) 2015, Chris Ford (christophertford at gmail)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(def namespace-declaration
  (macro/literally
    (ns geemee.live
      (:require [gamma.api :as g]))))

(def dependencies-cljs
  "The bundle of cljs dependencies."
  (macro/sources-cljs gamma.api gamma.ast))

(def dependencies-clj
  "The bundle of clj (macros) dependencies."
  (macro/sources-clj gamma.api))

;; WTF? see https://github.com/rogerallen/geemee/issues/2
;; suppress useless Google Closure error about duplicate provides
(set! (.-isProvided_ js/goog) (fn [name] false))

(defn loader
  "A namespace loader that looks in the dependencies bundle for required namespaces."
  [{:keys [name macros path]} callback]
  (let [[source of-type] (if macros
                           [(dependencies-clj  (.-str name)) "clj"]
                           [(dependencies-cljs (.-str name)) "cljs"])]
    (if source
      (js/console.log (str "Loading: " name " " of-type))
      (js/console.log (str "Unable to load: " name " " of-type)))
    (callback {:lang :clj :source source})))

(defn normalise [result]
  (update result :error #(some-> % .-cause .-stack))) ;; .-message

(defn uate-str
  "Evaluate a string of Clojurescript."
  [state expr-str]
  (cljs/eval-str
    state
    (str namespace-declaration expr-str)
    nil
    {:eval cljs/js-eval
     :load loader}
    normalise))

;; this isn't working yet...use uate-str instead
(defn uate
  "Evaluate a Clojurescript form."
  [state expr]
  (cljs/eval
    state
    (list '(ns geemee.live (:require [gamma.api :as g]))
          expr)
    {:eval cljs/js-eval
     :load loader}
    normalise))

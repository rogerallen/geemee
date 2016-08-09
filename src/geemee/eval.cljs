(ns geemee.eval
  (:require [cljs.js :as cljs])
  (:require-macros [geemee.macros :as macro]))

;; another wholesale ripoff of klangmeister
;; https://github.com/ctford/klangmeister/blob/master/src/klangmeister/compile/eval.cljs

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

;; FIXME? this isn't working yet.
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

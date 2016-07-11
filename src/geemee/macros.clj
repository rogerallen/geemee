(ns geemee.macros
  (:require [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.file :as file]
            [clojure.java.io :as io]
            [clojure.java.classpath :as classpath])
  (:import (java.io File)))

;; grabbed wholesale from klangmeister
;; https://github.com/ctford/klangmeister/blob/master/src/klangmeister/compile/macros.clj
;; updated to handle gamma macros

(def find_js {:extensions (list ".js")})

(defn name-val [rdr]
  [(-> rdr file/read-file-ns-decl second str) (slurp rdr)])

(defn files
  "find files of-type [find/cljs find/clj or find_js] in your source tree"
  [of-type]
  (->> (classpath/classpath-directories)
       (mapcat #(find/find-sources-in-dir % of-type))))

;;(spit "foo_files_js.txt" (pr-str (files find_js)))

(defn jars
  "find files of-type [find/cljs find/clj or find_js] in your jars"
  [of-type]
  (->> (classpath/classpath-jarfiles)
       (mapcat #(find/sources-in-jar % of-type))
       (map io/resource)))

;;(spit "foo_jars_clj.txt" (pr-str (jars find/clj)))
;;(spit "foo_jars_js.txt" (pr-str (jars find_js)))

(defn collate [entries]
  (reduce conj {} entries))

(defn sources* [of-type names]
  (let [in-names? (->> names (map str) set)
        relevant? (fn [[name _]] (in-names? name))]
    (->> (concat (jars of-type) (files of-type))
         (map name-val)
         (filter relevant?)
         collate)))

;;(spit "foo_sources.txt" (sources* (list 'gamma.api)))

(defmacro sources-cljs
  "Make a map of namespace name to source, looking for files on the classpath and in jars."
  [& names]
  (sources* find/cljs names))

(defmacro sources-clj
  "Make a map of namespace name to source, looking for files on the classpath and in jars."
  [& names]
  (sources* find/clj names))

(defmacro literally
  "Convert an expression into a string. Useful to allow syntax highlighting of inline CLJS."
  [expr]
  (str expr))

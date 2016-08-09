(ns geemee.macros
  (:require [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.file :as file]
            [clojure.java.io :as io]
            [clojure.java.classpath :as classpath])
  (:import (java.io File)))

;; This file's code was initially brought over from klangmeister.
;; So, I'm including the required license.
;; The code has been updated to handle gamma's macros & the clj-type of file loading.
;;
;; https://github.com/ctford/klangmeister/blob/master/src/klangmeister/compile/macros.clj
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

(def find_js {:extensions (list ".js")})

(defn name-val [rdr]
  [(-> rdr file/read-file-ns-decl second str) (slurp rdr)])

(defn files
  "find files of-type [find/cljs find/clj or find_js] in your source tree"
  [of-type]
  (->> (classpath/classpath-directories)
       (mapcat #(find/find-sources-in-dir % of-type))))

(defn jars
  "find files of-type [find/cljs find/clj or find_js] in your jars"
  [of-type]
  (->> (classpath/classpath-jarfiles)
       (mapcat #(find/sources-in-jar % of-type))
       (map io/resource)))

(defn collate [entries]
  (reduce conj {} entries))

(defn sources* [of-type names]
  (let [in-names? (->> names (map str) set)
        relevant? (fn [[name _]] (in-names? name))]
    (->> (concat (jars of-type) (files of-type))
         (map name-val)
         (filter relevant?)
         collate)))

(defmacro sources-cljs
  "Make a map of CLJS namespace name to source, looking for files on
  the classpath and in jars."
  [& names]
  (sources* find/cljs names))

(defmacro sources-clj
  "Make a map of CLJ namespace name to source, looking for files on
  the classpath and in jars."
  [& names]
  (sources* find/clj names))

(defmacro literally
  "Convert an expression into a string. Useful to allow syntax
  highlighting of inline CLJS."
  [expr]
  (str expr))

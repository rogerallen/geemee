(ns geemee.core
  (:require [gamma.api     :as g]
            [gamma.program :as p]
            [goog.dom      :as dom]
            [goog.webgl    :as wgl]
            [geemee.gee    :as gee]
            [cljs.js       :as cljs])
  (:require-macros [geemee.macros :as macro]))

(enable-console-print!)

;;(print "gee?" gee/MAX-GOOD-CODE-ATTEMPTS)
;;(print "gee" (gee/random-code 10))

;; ======================================================================
;; starting fragment shader
(defn start-rgb-fn [pos]
  (let [r (g/+  (g/* (g/swizzle pos :x) (g/swizzle pos :x))
                (g/* (g/swizzle pos :y) (g/swizzle pos :y)))]
    (g/vec3 (g/sin (g/* 19 r))
            (g/cos (g/* 13 r))
            (g/sin (g/*  7 r)))))
;; ======================================================================
;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:status-text "Hello world!"
                          :init   false
                          :width  720
                          :height 720
                          :rgb-fn start-rgb-fn}))

;; this state doesn't care about being overwritten on reload
(def vertex-position     (g/attribute "a_VertexPosition" :vec2))
(def vertex-shader       {(g/gl-position) (g/vec4 vertex-position 0 1)})
(def err-fragment-shader {(g/gl-frag-color) (g/vec4 1.0 0.0 0.0 1.0)})
(defn my-frag-color
  "wrapper to setup pos variable and call the rgbf"
  [rgb-fn w h]
  (let [tmp (g/div (g/gl-frag-coord) (g/vec4 w h 1.0 1.0))
        pos (g/swizzle tmp :xy)]
    (try
      (g/vec4 (rgb-fn pos) 1) ;; must have alpha=1 or you won't see it
      (catch :default e
        (println e)
        (println "Error rgb-fn" rgb-fn)
        (swap! app-state assoc :status-text (str e " rgb-fn " rgb-fn))
        (g/vec4 1 0 0 1)))))

;; ======================================================================
;; another wholesale ripoff of klangmeister
;; https://github.com/ctford/klangmeister/blob/master/src/klangmeister/compile/eval.cljs

(def namespace-declaration
  (macro/literally
    (ns geemee.live
      (:require gamma.api)
      )))

(def dependencies-cljs
  "A bundle of dependencies."
  (macro/sources-cljs
   gamma.api
   gamma.ast
   ))

(def dependencies-clj
  "A bundle of dependencies."
  (macro/sources-clj
   gamma.api
   ))

;;(spit "foo_dependencies.txt" dependencies)

(defn loader
  "A namespace loader that looks in the dependencies bundle for required namespaces."
  [{:keys [name macros path]} callback]
  (let [str-name (.-str name)
        source (if macros
                 (dependencies-clj str-name)
                 (dependencies-cljs str-name))
        of-type (if macros "clj" "cljs")
        lang :clj]
    (if source
      (js/console.log (str "Loading: " str-name " " of-type))
      (js/console.log (str "Unable to load: " str-name " " of-type)))
    (callback {:lang lang :source (str source)})))

(def state
  "A compiler state, which is shared across compilations."
  (cljs/empty-state))

(set-print-err-fn! #(js/console.log))

(defn normalise [result]
  (update result :error #(some-> % .-cause .-message)))

(defn uate
  "Evaluate a string of Clojurescript, with synthesis and music namespaces available."
  [expr-str]
  (cljs/eval-str
    state
    (str namespace-declaration expr-str)
    nil
    {:eval cljs/js-eval
     :load loader}
    normalise))

;; ======================================================================
;; initialize & display a random code...

;;(println "starting  uate...")
;;(println "B" (uate "(fn [x] (+ x 1))")) ;; anon fns don't work ???
;;(println "C" (uate "(defn foo [x] (+ x 1))")) ;; real fns do!
;;(println "done uate")

;;(def xxx (g/cos 1.9178))
;;(println "a" (g/vec3 0))

;; BIG DEAL FIXME...
;; okay, what I see is that if I get into a bad state, then I can't
;; recover from it.  Will have to figure that out.

(defn get-rgb-fn []
  (let [;;rgb-fn start-rgb-fn              ;; ok
        ;;rgb-fn (fn [pos] (g/vec3 pos 0)) ;; ok
        ;;rgb-fn (defn my-fn [pos] (g/vec3 pos 1)) ;; ok
        ;;rgb-fn (fn [pos] (g/cos 1.9))    ;; bad
        ;;rgb-fn (:value (uate "(defn my-fn [pos] (gamma.api/vec3 0 pos))"))
        ;;rgb-fn (:value (uate "(defn my-fn [pos] (gamma.api/cos 1.9))"))
        ;; THE REAL CALL
        random-code (str (gee/get-random-code))
        ;;_ (print "random code: " random-code)
        rgb-fn (:value (uate (str "(defn my-fn [pos] " random-code ")")))
        ;;rgb-fn (defn my-fn [pos] (gamma.api/sqrt (gamma.api/vec3 0.5025 1.0828 -2.3827)))
        ;;rgb-fn (defn my-fn [pos] (g/ (g/vec3 0.5 0.5 0.5) (g/vec3 0.25 0.1 -0.25)))
        ;;_ (println "A rgb-fn" rgb-fn)
        ]
    rgb-fn))

(defn init []
  (swap! app-state assoc
         :status-text "ok"
         :width 360 :height 360
         ;;:width 720 :height 720
         :rgb-fn (get-rgb-fn)))

(defn render [gl fragment-shader]
  (let [prog (p/program {:vertex-shader vertex-shader
                       :fragment-shader fragment-shader
                       :precision {:float :highp}
                       })
        vs  (.createShader gl wgl/VERTEX_SHADER)
        fs  (.createShader gl wgl/FRAGMENT_SHADER)
        pgm (.createProgram gl)
        xs  (js/Float32Array. #js [-1 -1 ;; bottom tri
                                    1 -1
                                   -1  1
                                   -1  1 ;; top tri
                                    1 -1
                                    1  1])
        buf (.createBuffer gl)
        ;; huh? _ (swap! app-state assoc :status-text "ok")
        ]
    (.shaderSource gl vs (-> prog :vertex-shader :glsl))
    (.compileShader gl vs)
    (if-not (.getShaderParameter gl vs wgl/COMPILE_STATUS)
      (do
        (swap! app-state assoc :status-text (.getShaderInfoLog gl vs))
        (print (.getShaderInfoLog gl vs))
        (println "src:" (-> prog :vertex-shader :glsl))
        (render gl err-fragment-shader))
      (do
        (.shaderSource gl fs (-> prog :fragment-shader :glsl))
        (.compileShader gl fs)
        (if-not (.getShaderParameter gl fs wgl/COMPILE_STATUS)
          (do
            (swap! app-state assoc :status-text (.getShaderInfoLog gl fs))
            (print (.getShaderInfoLog gl fs))
            (println "src:" (-> prog :fragment-shader :glsl))
            (render gl err-fragment-shader))
          (do
            (.attachShader gl pgm vs)
            (.attachShader gl pgm fs)
            (.linkProgram gl pgm)
            (if-not (.getProgramParameter gl pgm wgl/LINK_STATUS)
              (do
                (swap! app-state assoc :status-text (.getProgramInfoLog gl pgm))
                (print "ERROR PGM:" (.getProgramInfoLog gl pgm))
                (render gl err-fragment-shader))
              (do
                (.bindBuffer gl wgl/ARRAY_BUFFER buf)
                (.bufferData gl wgl/ARRAY_BUFFER xs wgl/STATIC_DRAW)
                (.enableVertexAttribArray
                 gl (.getAttribLocation gl pgm (:name vertex-position)))
                (.vertexAttribPointer
                 gl (.getAttribLocation gl pgm (:name vertex-position))
                 2 wgl/FLOAT false 0 0)
                (.useProgram gl pgm)
                (.drawArrays gl wgl/TRIANGLES 0 6)))))))));;)

(defn update-status [e]
  (set! (.-innerHTML e) (@app-state :status-text)))

(defn draw-new-image []
  (init)
  (let [_ (print "======================================================================\nlet draw-new-image")
        canvas (dom/getElement "gl-canvas")
        _      (goog.dom.setProperties canvas
                                       (clj->js {:width (@app-state :width)
                                                 :height (@app-state :height)}))
        gl     (.getContext canvas "webgl")
        status (dom/getElement "status")]
    (render gl {(g/gl-frag-color) (my-frag-color (@app-state :rgb-fn)
                                                 (@app-state :width)
                                                 (@app-state :height))})
    (update-status status)))

(if (@app-state :init)
  (draw-new-image)
  (let [button (dom/getElement "update-btn")
        _      (.addEventListener button "click" draw-new-image)
        _      (swap! app-state assoc :init true)]
    (draw-new-image)))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

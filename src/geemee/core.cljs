(ns geemee.core
  (:require [gamma.api     :as g]
            [gamma.program :as p]
            [goog.dom      :as dom]
            [goog.webgl    :as wgl]
            [geemee.gee    :as gee]
            [geemee.eval   :as eval]
            [cljs.js       :as cljs]
            [fipp.clojure  :as fc]))

(enable-console-print!)
(set-print-err-fn! #(js/console.log))

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
(defonce app-state (atom {:status-text "Click Update!"
                          :init        false
                          :width       300
                          :height      300
                          :cur-code    nil
                          :cur-pretty-code nil
                          :rgb-fn      nil
                          :uate-state  nil}))

;; ======================================================================
(defn app-status! [& args]
  (swap! app-state assoc
         :status-text (str "<pre>" (apply str args) "</pre>")))
(defn status-html! [e]
  (set! (.-innerHTML e) (@app-state :status-text)))

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
        (app-status! e "\n" (:cur-pretty-code @app-state))
        (g/vec4 1 0 0 1)))))

;; ======================================================================
;; check uate output
(defn validate-uate! [rgb-fn pretty-full-code]
  (if (:error rgb-fn)
    (do
      (println "evaluate error:" (:error rgb-fn))
      (app-status! "Evaluation error: " (:error rgb-fn) "\n"
                   pretty-full-code)
      (fn [pos] (g/vec3 1.0 0.0 0.0)))
    (:value rgb-fn)))

(defn code2rgb-fn! [random-code]
  (let [full-code        (list 'defn 'pixel '[pos] random-code)
        full-code-str    (str full-code)
        pretty-full-code (-> full-code fc/pprint with-out-str)
        _                (app-status! pretty-full-code)
        _                (swap! app-state assoc
                                :cur-pretty-code pretty-full-code)
        rgb-fn           (eval/uate-str (:uate-state @app-state) full-code-str)
        rgb-fn           (validate-uate! rgb-fn pretty-full-code)]
    rgb-fn))

;; initialize & display a random code...
(defn set-rgb-fn! []
  (let [random-code (gee/get-random-code)]
    (swap! app-state assoc
           :cur-code random-code
           :rgb-fn (code2rgb-fn! random-code))))

(defn mutate-rgb-fn! []
  (let [random-code (gee/mutate (:cur-code @app-state))]
    (swap! app-state assoc
           :cur-code random-code
           :rgb-fn (code2rgb-fn! random-code))))

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
        (app-status! (.getShaderInfoLog gl vs))
        (print (.getShaderInfoLog gl vs))
        (println "src:" (-> prog :vertex-shader :glsl))
        (render gl err-fragment-shader))
      (do
        (.shaderSource gl fs (-> prog :fragment-shader :glsl))
        (.compileShader gl fs)
        (if-not (.getShaderParameter gl fs wgl/COMPILE_STATUS)
          (do
            (app-status! "Compile Error:\n" (.getShaderInfoLog gl fs) "\n" (:cur-pretty-code @app-state))
            (print (.getShaderInfoLog gl fs))
            (println "src:" (-> prog :fragment-shader :glsl))
            (render gl err-fragment-shader))
          (do
            (.attachShader gl pgm vs)
            (.attachShader gl pgm fs)
            (.linkProgram gl pgm)
            (if-not (.getProgramParameter gl pgm wgl/LINK_STATUS)
              (do
                (app-status! "Link Error:\n" (.getProgramInfoLog gl pgm) "\n" (:cur-pretty-code @app-state))
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

(defn draw-image []
  (let [canvas (dom/getElement "gl-canvas")
        _      (goog.dom.setProperties canvas
                                       (clj->js {:width (@app-state :width)
                                                 :height (@app-state :height)}))
        gl     (.getContext canvas "webgl")
        status (dom/getElement "status")]
    (render gl {(g/gl-frag-color) (my-frag-color (@app-state :rgb-fn)
                                                 (@app-state :width)
                                                 (@app-state :height))})
    (status-html! status)))

(defn draw-new-image! []
  (set-rgb-fn!)
  (draw-image))

(defn update-click! []
  (draw-new-image!))

(defn mutate-click! []
  (mutate-rgb-fn!)
  (draw-image))

(defn after-load []
  ;;(println "after-load")
  (if (@app-state :init)
    (draw-new-image!)
    (let [;;_ (println "not init")
          button (dom/getElement "generate-btn")
          _      (.addEventListener button "click" update-click!)
          button (dom/getElement "mutate-btn")
          _      (.addEventListener button "click" mutate-click!)
          _      (swap! app-state assoc
                        :uate-state (cljs/empty-state)
                        :init true)]
      (draw-new-image!))))

(set! (.-onload js/window)
      (after-load))

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

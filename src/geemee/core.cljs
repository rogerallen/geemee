(ns geemee.core
  (:require [gamma.api :as g]
            [gamma.program :refer [program]]
            [goog.dom :as gdom]
            [goog.webgl :as wgl]))

(enable-console-print!)

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
                          :width  720
                          :height 720
                          :rgb-fn start-rgb-fn}))

;; this state doesn't care about being overwritten on reload
(def vertex-position     (g/attribute "a_VertexPosition" :vec2))
(def vertex-shader       {(g/gl-position) (g/vec4 vertex-position 0 1)})
(def err-fragment-shader {(g/gl-frag-color) (g/vec4 1.0 0.0 0.0 1.0)})
(defn my-frag-color
  "wrapper to setup pos variable and call the rgbf"
  [rgbf w h]
  (let [tmp (g/div (g/gl-frag-coord) (g/vec4 w h 1.0 1.0))
        pos (g/swizzle tmp :xy)]
    (g/vec4 (rgbf pos) 1))) ;; must have alpha=1 or you won't see it

;; ======================================================================
(defn init []
  (let [rgb-fn start-rgb-fn]
    (swap! app-state assoc
           :status-text "ok"
           :width 720 :height 720
           :rgb-fn rgb-fn)))

(defn render [gl fragment-shader]
  (let [prog (program {:vertex-shader vertex-shader
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

(defn main []
  (init)
  (let [canvas (gdom/getElement "gl-canvas")
        _      (goog.dom.setProperties canvas
                                       (clj->js {:width (@app-state :width)
                                                 :height (@app-state :height)}))

        gl     (.getContext canvas "webgl")
        status (gdom/getElement "status")]
    (render gl {(g/gl-frag-color) (my-frag-color (@app-state :rgb-fn)
                                                 (@app-state :width)
                                                 (@app-state :height))})
    (update-status status)))

(main)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

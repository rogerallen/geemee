(ns geemee.core
  (:require [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as wgl]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

;; this state doesn't care about being overwritten on reload
(def vertex-position (g/attribute "a_VertexPosition" :vec2))
(def vertex-shader   {(g/gl-position) (g/vec4 vertex-position 0 1)})
(def fragment-shader {(g/gl-frag-color) (g/vec4 1 0.25 0.5 1)})

(defn main []
  (let [prog (p/program {:vertex-shader vertex-shader
                         :fragment-shader fragment-shader})
        gl  (.getContext (gdom/getElement "gl-canvas") "webgl")
        vs  (.createShader gl wgl/VERTEX_SHADER)
        fs  (.createShader gl wgl/FRAGMENT_SHADER)
        pgm (.createProgram gl)
        xs  (js/Float32Array. #js [-1 -1 ;; bottom tri
                                    1 -1
                                   -1  1
                                   -1  1 ;; top tri
                                    1 -1
                                    1  1])
        buf (.createBuffer gl)]
    (doto gl
      (.shaderSource vs (-> prog :vertex-shader :glsl))
      (.compileShader vs)
      (.shaderSource fs (-> prog :fragment-shader :glsl))
      (.compileShader fs)
      (.attachShader pgm vs)
      (.attachShader pgm fs)
      (.linkProgram pgm)
      (.bindBuffer wgl/ARRAY_BUFFER buf)
      (.bufferData wgl/ARRAY_BUFFER xs wgl/STATIC_DRAW)
      (.enableVertexAttribArray (.getAttribLocation gl pgm (:name vertex-position)))
      (.vertexAttribPointer (.getAttribLocation gl pgm (:name vertex-position))
                            2 wgl/FLOAT false 0 0)
      (.useProgram pgm)
      (.drawArrays wgl/TRIANGLES 0 6))))

(main)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

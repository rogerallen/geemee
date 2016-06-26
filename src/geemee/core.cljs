(ns geemee.core
  (:require [gamma.api :as g]
            [gamma.program :as p]
            [goog.dom :as gdom]
            [goog.webgl :as wgl]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
(defonce app-state (atom {:text "Hello world!"}))

;;(println (g/aget (g/gl-frag-coord) 0)  )

;; this state doesn't care about being overwritten on reload
(def vertex-position (g/attribute "a_VertexPosition" :vec2))
(def vertex-shader   {(g/gl-position) (g/vec4 vertex-position 0 1)})
(def fragment-shader
  {
   (g/gl-frag-color)
   (let [width 500.0
         height 500.0
         c (g/div (g/gl-frag-coord) (g/vec4 width height 1.0 1.0))
         x (g/swizzle c :x)
         y (g/swizzle c :y)
         ]
     (g/vec4 x y 0 1)
     ) ;; must have alpha=1 or you won't see it
   })


(defn main []
  (let [prog (p/program {:vertex-shader vertex-shader
                         :fragment-shader fragment-shader
                         :precision {:float :highp}})
        ;;_   (println (-> prog :vertex-shader :glsl))
        _   (println (-> prog :fragment-shader :glsl))
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
    ;;(doto gl
    (.shaderSource gl vs (-> prog :vertex-shader :glsl))
    ;;(println "Ea" (.getError gl))
    (.compileShader gl vs)
    (when-not (.getShaderParameter gl vs wgl/COMPILE_STATUS)
      (print "VS:" (.getShaderInfoLog gl vs)))
    (.shaderSource gl fs (-> prog :fragment-shader :glsl))
    (.compileShader gl fs)
    (when-not (.getShaderParameter gl fs wgl/COMPILE_STATUS)
      (print "FS:" (.getShaderInfoLog gl fs)))
    (.attachShader gl pgm vs)
    (.attachShader gl pgm fs)
    (.linkProgram gl pgm)
    (when-not (.getProgramParameter gl pgm wgl/LINK_STATUS)
      (print "ERROR PGM:" (.getProgramInfoLog gl pgm)))
    (.bindBuffer gl wgl/ARRAY_BUFFER buf)
    (.bufferData gl wgl/ARRAY_BUFFER xs wgl/STATIC_DRAW)
    (.enableVertexAttribArray gl (.getAttribLocation gl pgm (:name vertex-position)))
    (.vertexAttribPointer gl (.getAttribLocation gl pgm (:name vertex-position))
                          2 wgl/FLOAT false 0 0)
    (.useProgram gl pgm)
    (.drawArrays gl wgl/TRIANGLES 0 6)));;)

(main)

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

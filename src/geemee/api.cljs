(ns geemee.api
  (:require [gamma.api :as g]))

;; term-fns
;;    noise, turbulance, etc.
;; unary-fns
;;    `square `vsqrt `sigmoid `max-component `min-component
;;    `length `normalize `gradient
;;    `hue-from-rgb `lightness-from-rgb `saturation-from-rgb
;;    `hsl-from-rgb `red-from-hsl `green-from-hsl `blue-from-hsl
;;    `rgb-from-hsl `x `y `z `t `alpha
;; binary-fns
;;    `vpow `vmod `dot `cross3
;;    `vmin `vmax `checker `scale `offset
;;    `adjust-hue `adjust-hsl `vconcat})

;; ======================================================================
;; Translated noise glsl to clojure from
;; https://github.com/ashima/webgl-noise/blob/master/src/noise3D.glsl
;;
;; Description : Array and textureless GLSL 2D/3D/4D simplex
;;               noise functions.
;;      Author : Ian McEwan, Ashima Arts.
;;  Maintainer : stegu
;;     Lastmod : 20110822 (ijm)
;;     License : Copyright (C) 2011 Ashima Arts. All rights reserved.
;;               Distributed under the MIT License. See LICENSE file.
;;               https://github.com/ashima/webgl-noise
;;               https://github.com/stegu/webgl-noise
;;

(defn mod289 [x]
  (g/- x (g/* (g/floor (g/* x (g/div 1.0 289.0))) 289.0)))

(defn permute [x]
  (mod289 (g/* (g/+ (g/* x (g/vec4 34.0)) (g/vec4 1.0)) x)))

(defn taylor-inv-sqrt [r]
  (g/- (g/vec4 1.79284291400159) (g/* (g/vec4 0.85373472095314) r)))

(defn snoise [v] ;; v should be vec3
  (let [C  (g/vec2 0.16666666666666666 0.3333333333333333)
        D  (g/vec4 0.0, 0.5, 1.0, 2.0)
        ;; First corner
        i  (g/floor (g/+ v (g/vec3 (g/dot v (g/swizzle C :yyy)))))
        x0 (g/+ (g/- v i) (g/vec3 (g/dot i (g/swizzle C :xxx))))
        ;; Other corners
        g  (g/step (g/swizzle x0 :yzx) (g/swizzle x0 :xyz))
        l  (g/- (g/vec3 1.0) g)
        i1 (g/min g (g/swizzle l :zxy))
        i2 (g/max g (g/swizzle l :zxy))

        ;;   x0 = x0 - 0.0 + 0.0 * C.xxx;
        ;;   x1 = x0 - i1  + 1.0 * C.xxx;
        ;;   x2 = x0 - i2  + 2.0 * C.xxx;
        ;;   x3 = x0 - 1.0 + 3.0 * C.xxx;
        x1 (g/+ (g/- x0 i1) (g/swizzle C :xxx))
        x2 (g/+ (g/- x0 i2) (g/swizzle C :yyy)) ;; 2.0*C.x = 1/3 = C.y
        x3 (g/- x0 (g/swizzle D :yyy))        ;; -1.0+3.0*C.x = -0.5 = -D.y
        ;; Permutations
        i  (mod289 i)
        p  (permute (g/+
                     (permute (g/+
                               (permute (g/+ (g/swizzle i :zzzz)
                                             (g/vec4 0.0 (g/swizzle i1 :z) (g/swizzle i2 :z) 1.0)))
                               (g/+ (g/swizzle i :yyyy)
                                    (g/vec4 0.0 (g/swizzle i1 :y) (g/swizzle i2 :y) 1.0))))
                     (g/+ (g/swizzle i :xxxx)
                          (g/vec4 0.0 (g/swizzle i1 :x) (g/swizzle i2 :x) 1.0))))
        ;; Gradients: 7x7 points over a square, mapped onto an octahedron.
        ;; The ring size 17*17 = 289 is close to a multiple of 49 (49*6 = 294)
        n_ 0.142857142857; ;; 1.0/7.0
        ns (g/- (g/* n_ (g/swizzle D :wyz)) (g/swizzle D :xzx))
        j  (g/- p (g/* (g/vec4 49.0) (g/floor (g/* p (g/* (g/swizzle ns :zzzz) (g/swizzle ns :zzzz)))))) ;;  mod(p,7*7)
        x_ (g/floor (g/* j (g/swizzle ns :zzzz)))
        y_ (g/floor (g/- j (g/* (g/vec4 7.0) x_))) ;; mod(j,N)

        x  (g/+ (g/* x_ (g/swizzle ns :xxxx)) (g/swizzle ns :yyyy))
        y  (g/+ (g/* y_ (g/swizzle ns :xxxx)) (g/swizzle ns :yyyy))
        h  (g/- (g/- (g/vec4 1.0) (g/abs x)) (g/abs y))

        b0 (g/vec4 (g/swizzle x :xy) (g/swizzle y :xy))
        b1 (g/vec4 (g/swizzle x :zw) (g/swizzle y :zw))
        ;;vec4 s0 = vec4(lessThan(b0,0.0))*2.0 - 1.0;
        ;;vec4 s1 = vec4(lessThan(b1,0.0))*2.0 - 1.0;
        s0 (g/+ (g/* (g/floor b0) (g/vec4 2.0)) (g/vec4 1.0))
        s1 (g/+ (g/* (g/floor b1) (g/vec4 2.0)) (g/vec4 1.0))
        sh (g/* -1.0 (g/step h (g/vec4 0.0)))

        a0 (g/+ (g/swizzle b0 :xzyw) (g/* (g/swizzle s0 :xzyw) (g/swizzle sh :xxyy)))
        a1 (g/+ (g/swizzle b1 :xzyw) (g/* (g/swizzle s1 :xzyw) (g/swizzle sh :xxyy)))

        p0 (g/vec3 (g/swizzle a0 :xy) (g/swizzle h :x))
        p1 (g/vec3 (g/swizzle a0 :zw) (g/swizzle h :y))
        p2 (g/vec3 (g/swizzle a1 :xy) (g/swizzle h :z))
        p3 (g/vec3 (g/swizzle a1 :zw) (g/swizzle h :w))

        ;;Normalise gradients
        norm (taylor-inv-sqrt (g/vec4 (g/dot p0 p0) (g/dot p1 p1)
                                      (g/dot p2 p2) (g/dot p3 p3)))
        p0 (g/* p0 (g/swizzle norm :xxx))
        p1 (g/* p1 (g/swizzle norm :yyy))
        p2 (g/* p2 (g/swizzle norm :zzz))
        p3 (g/* p3 (g/swizzle norm :www))

        ;; Mix final noise value
        m  (g/max (g/- (g/vec4 0.6)
                       (g/vec4 (g/dot x0 x0) (g/dot x1 x1)
                               (g/dot x2 x2) (g/dot x3 x3)))
                  (g/vec4 0.0))
        m  (g/* m m)]
    (g/* 42.0
         (g/dot (g/* m m) (g/vec4 (g/dot p0 x0) (g/dot p1 x1)
                                  (g/dot p2 x2) (g/dot p3 x3))))))

#!/usr/bin/ol
(import (lib opengl))

(define Context (gl:Create "8. Teapot 2.0"))

(import (OpenGL version-2-0))

(define width 640)
(define height 480)

;(teapot)
(define vertices '(
    (  0.2000  0.0000 2.70000 ) (  0.2000 -0.1120 2.70000 )
    (  0.1120 -0.2000 2.70000 ) (  0.0000 -0.2000 2.70000 )
    (  1.3375  0.0000 2.53125 ) (  1.3375 -0.7490 2.53125 )
    (  0.7490 -1.3375 2.53125 ) (  0.0000 -1.3375 2.53125 )
    (  1.4375  0.0000 2.53125 ) (  1.4375 -0.8050 2.53125 )
    (  0.8050 -1.4375 2.53125 ) (  0.0000 -1.4375 2.53125 )
    (  1.5000  0.0000 2.40000 ) (  1.5000 -0.8400 2.40000 )
    (  0.8400 -1.5000 2.40000 ) (  0.0000 -1.5000 2.40000 )
    (  1.7500  0.0000 1.87500 ) (  1.7500 -0.9800 1.87500 )
    (  0.9800 -1.7500 1.87500 ) (  0.0000 -1.7500 1.87500 )
    (  2.0000  0.0000 1.35000 ) (  2.0000 -1.1200 1.35000 )
    (  1.1200 -2.0000 1.35000 ) (  0.0000 -2.0000 1.35000 )
    (  2.0000  0.0000 0.90000 ) (  2.0000 -1.1200 0.90000 )
    (  1.1200 -2.0000 0.90000 ) (  0.0000 -2.0000 0.90000 )
    ( -2.0000  0.0000 0.90000 ) (  2.0000  0.0000 0.45000 )
    (  2.0000 -1.1200 0.45000 ) (  1.1200 -2.0000 0.45000 )
    (  0.0000 -2.0000 0.45000 ) (  1.5000  0.0000 0.22500 )
    (  1.5000 -0.8400 0.22500 ) (  0.8400 -1.5000 0.22500 )
    (  0.0000 -1.5000 0.22500 ) (  1.5000  0.0000 0.15000 )
    (  1.5000 -0.8400 0.15000 ) (  0.8400 -1.5000 0.15000 )
    (  0.0000 -1.5000 0.15000 ) ( -1.6000  0.0000 2.02500 )
    ( -1.6000 -0.3000 2.02500 ) ( -1.5000 -0.3000 2.25000 )
    ( -1.5000  0.0000 2.25000 ) ( -2.3000  0.0000 2.02500 )
    ( -2.3000 -0.3000 2.02500 ) ( -2.5000 -0.3000 2.25000 )
    ( -2.5000  0.0000 2.25000 ) ( -2.7000  0.0000 2.02500 )
    ( -2.7000 -0.3000 2.02500 ) ( -3.0000 -0.3000 2.25000 )
    ( -3.0000  0.0000 2.25000 ) ( -2.7000  0.0000 1.80000 )
    ( -2.7000 -0.3000 1.80000 ) ( -3.0000 -0.3000 1.80000 )
    ( -3.0000  0.0000 1.80000 ) ( -2.7000  0.0000 1.57500 )
    ( -2.7000 -0.3000 1.57500 ) ( -3.0000 -0.3000 1.35000 )
    ( -3.0000  0.0000 1.35000 ) ( -2.5000  0.0000 1.12500 )
    ( -2.5000 -0.3000 1.12500 ) ( -2.6500 -0.3000 0.93750 )
    ( -2.6500  0.0000 0.93750 ) ( -2.0000 -0.3000 0.90000 )
    ( -1.9000 -0.3000 0.60000 ) ( -1.9000  0.0000 0.60000 )
    (  1.7000  0.0000 1.42500 ) (  1.7000 -0.6600 1.42500 )
    (  1.7000 -0.6600 0.60000 ) (  1.7000  0.0000 0.60000 )
    (  2.6000  0.0000 1.42500 ) (  2.6000 -0.6600 1.42500 )
    (  3.1000 -0.6600 0.82500 ) (  3.1000  0.0000 0.82500 )
    (  2.3000  0.0000 2.10000 ) (  2.3000 -0.2500 2.10000 )
    (  2.4000 -0.2500 2.02500 ) (  2.4000  0.0000 2.02500 )
    (  2.7000  0.0000 2.40000 ) (  2.7000 -0.2500 2.40000 )
    (  3.3000 -0.2500 2.40000 ) (  3.3000  0.0000 2.40000 )
    (  2.8000  0.0000 2.47500 ) (  2.8000 -0.2500 2.47500 )
    (  3.5250 -0.2500 2.49375 ) (  3.5250  0.0000 2.49375 )
    (  2.9000  0.0000 2.47500 ) (  2.9000 -0.1500 2.47500 )
    (  3.4500 -0.1500 2.51250 ) (  3.4500  0.0000 2.51250 )
    (  2.8000  0.0000 2.40000 ) (  2.8000 -0.1500 2.40000 )
    (  3.2000 -0.1500 2.40000 ) (  3.2000  0.0000 2.40000 )
    (  0.0000  0.0000 3.15000 ) (  0.8000  0.0000 3.15000 )
    (  0.8000 -0.4500 3.15000 ) (  0.4500 -0.8000 3.15000 )
    (  0.0000 -0.8000 3.15000 ) (  0.0000  0.0000 2.85000 )
    (  1.4000  0.0000 2.40000 ) (  1.4000 -0.7840 2.40000 )
    (  0.7840 -1.4000 2.40000 ) (  0.0000 -1.4000 2.40000 )
    (  0.4000  0.0000 2.55000 ) (  0.4000 -0.2240 2.55000 )
    (  0.2240 -0.4000 2.55000 ) (  0.0000 -0.4000 2.55000 )
    (  1.3000  0.0000 2.55000 ) (  1.3000 -0.7280 2.55000 )
    (  0.7280 -1.3000 2.55000 ) (  0.0000 -1.3000 2.55000 )
    (  1.3000  0.0000 2.40000 ) (  1.3000 -0.7280 2.40000 )
    (  0.7280 -1.3000 2.40000 ) (  0.0000 -1.3000 2.40000 )))

(define (nth list n)
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))

(define Rim: '(
    ( 102 103 104 105   4   5   6   7
        8   9  10  11  12  13  14  15 )))
(define Body: '(
    (  12  13  14  15  16  17  18  19
       20  21  22  23  24  25  26  27 )
    (  24  25  26  27  29  30  31  32
       33  34  35  36  37  38  39  40 )))
(define Lid: '(
    (  96  96  96  96  97  98  99 100
      101 101 101 101   0   1   2   3 )
    (   0   1   2   3 106 107 108 109
      110 111 112 113 114 115 116 117 )))
(define Handle: '(
    (  41  42  43  44  45  46  47  48
       49  50  51  52  53  54  55  56 )
    (  53  54  55  56  57  58  59  60
       61  62  63  64  28  65  66  67 )))
(define Spout: '(
    (  68  69  70  71  72  73  74  75
       76  77  78  79  80  81  82  83 )
    (  80  81  82  83  84  85  86  87
       88  89  90  91  92  93  94  95 )))

(define knots '(0 0 0 0 1 1 1 1))

(define (compile-shader)
(let ((po (gl:CreateProgram
"#version 120 // OpenGL 2.1
   varying vec2 xy;
   void main() {
      gl_Position = ftransform(); // - vec4(1.0, 1.0, 0.0, 0.0); // gl_ModelViewProjectionMatrix * gl_Vertex
      xy = normalize(gl_NormalMatrix * gl_Normal).xy / 1.0;
      }"

"#version 120 // OpenGL 2.1
   // http://glslsandbox.com/e#19102.0
   uniform float time;

   #define iterations 14
   #define formuparam 0.530

   #define volsteps 18
   #define stepsize 0.2

   #define zoom   0.800
   #define tile   0.850
   #define speed  0.0001

   #define brightness 0.0015
   #define darkmatter 0.400
   #define distfading 0.760
   #define saturation 0.800

   varying vec2 xy;
   void main(void) {
      vec2 viewport = vec2(1280, 720);

      //get coords and direction
      vec2 uv=xy;//gl_FragCoord.xy / viewport.xy - .5;
      uv.y*=viewport.y/viewport.x;
      vec3 dir=vec3(uv*zoom,1.);

      float a2=speed+.5;
      float a1=0.0;
      mat2 rot1=mat2(cos(a1),sin(a1),-sin(a1),cos(a1));
      mat2 rot2=rot1;//mat2(cos(a2),sin(a2),-sin(a2),cos(a2));
      dir.xz*=rot1;
      dir.xy*=rot2;

      vec3 from=vec3(-0.05, 0.05, 0);
      //from.x-=time; <- movement

      from.z = time / 20000.0;

      from.x-=0.2;//mouse.x;
      from.y-=0.7;//mouse.y;

      from.xz*=rot1;
      from.xy*=rot2;

      //volumetric rendering
      float s=.4,fade=.2;
      vec3 v=vec3(0.4);
      for (int r=0; r<volsteps; r++) {
         vec3 p=from+s*dir*.5;
         p = abs(vec3(tile)-mod(p,vec3(tile*2.))); // tiling fold
         float pa,a=pa=0.;
         for (int i=0; i<iterations; i++) {
            p=abs(p)/dot(p,p)-formuparam; // the magic formula
            a+=abs(length(p)-pa); // absolute sum of average change
            pa=length(p);
         }
         float dm=max(0.,darkmatter-a*a*.001); //dark matter
         a*=a*a*2.; // add contrast
         if (r>3) fade*=1.-dm; // dark matter, don't render near
         //v+=vec3(dm,dm*.5,0.);
         v+=fade;
         v+=vec3(s,s*s,s*s*s*s)*a*brightness*fade; // coloring based on distance
         fade*=distfading; // distance fading
         s+=stepsize;
      }
      v=mix(vec3(length(v)),v,saturation); //color adjust
      gl_FragColor = vec4(v*.01,1.);
      //gl_FragColor = vec4(xy.x, xy.y, 0, 1.0);
      }")))
po))

(gl:run Context

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glEnable GL_DEPTH_TEST)
   (glEnable GL_AUTO_NORMAL)

   (let ((po (compile-shader))
         (teapot (gluNewNurbsRenderer)))
      ;(gluNurbsProperty teapot GLU_DISPLAY_MODE GLU_OUTLINE_POLYGON)

   (list 1 0.02 3 0.03  po teapot)))

; draw
(lambda (x   dx y   dy  po teapot)
   (glClear (vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
   (glUseProgram po)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 100)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x y 8
      0 0 0
      0 1 0)

   (let ((render (lambda (surface)
                     (gluBeginSurface teapot)
                     (gluNurbsSurface teapot 8 knots 8 knots (* 4 3) 3  (fold append '() (map (lambda (n) (nth vertices n)) surface))  4 4 GL_MAP2_VERTEX_3)
                     (gluEndSurface teapot))))
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)
      (glScalef -1 1  1)
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)
      (glScalef -1 -1 1)
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)
      (glScalef -1 1  1)
      (for-each render Rim:)
      (for-each render Body:)
      (for-each render Lid:)

      (for-each render Handle:)
      (for-each render Spout:)
      (glScalef 1 -1 1)
      (for-each render Handle:)
      (for-each render Spout:))

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (ny (if (or (> y 4) (< y -4)) (- dy) dy)))
      (list (+ x nx) nx (+ y ny) ny  po teapot))))

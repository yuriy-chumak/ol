#!/usr/bin/ol

(import (OpenGL version-1-0)
   (lib x11) (owl io))

(define width 640)
(define height 480)

;(main)
(define display (XOpenDisplay 0))
(define screen (XDefaultScreen display))

(define vi (glXChooseVisual display screen
   (raw type-vector-raw '(
      4 0 0 0 ; GLX_RGBA
      5 0 0 0  1 0 0 0 ; GLX_DOUBLEBUFFER
      8 0 0 0  1 0 0 0 ; GLX_RED_SIZE
      9 0 0 0  1 0 0 0 ; GLX_GREEN_SIZE
     10 0 0 0  1 0 0 0 ; GLX_BLUE_SIZE
     12 0 0 0  1 0 0 0 ; GLX_DEPTH_SIZE

      0 0 0 0)))); None
(define cx (glXCreateContext display vi 0 1))

(define window (XCreateSimpleWindow display (XRootWindow display screen)
   0 0 width height 1
   (XBlackPixel display screen) (XWhitePixel display screen)))
   
(XSelectInput display window ExposureMask)
(XMapWindow display window)


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
    
;(print
;(fold append '() vertices))
    
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

;(init)
(glXMakeCurrent display window cx)

(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ width height) 0.1 100)

(glEnable GL_DEPTH_TEST)

(define teapot (gluNewNurbsRenderer))
(gluNurbsProperty teapot GLU_DISPLAY_MODE GLU_OUTLINE_POLYGON)

(glXMakeCurrent display null null)

;(loop)
(let ((XEvent (raw type-vector-raw (repeat 0 192))))
(let loop ((x 1) (dx 0.02) (z 3) (dz 0.03))
   (let process-events ()
      (if (> (XPending display) 0)
         (begin
            (XNextEvent display XEvent)
            (process-events))))

      (glXMakeCurrent display window cx)
   (glClear (fx:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x 8 z
      0 0 0
      0 1 0)

   ; teapot
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


   (glXSwapBuffers display window)
   (glXMakeCurrent display null null)

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (nz (if (or (> z 4) (< z -4)) (- dz) dz)))
      (loop (+ x nx) nx (+ z nz) nz))))

;(done)
(print "Ok.")

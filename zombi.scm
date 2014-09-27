(define skeleton-bones (list->ff '(
  ( 0 "Bip01" -1)
  ( 1 "Bip01 Pelvis" 0)
  ( 2 "Bip01 L Thigh" 1)
  ( 3 "Bip01 L Calf" 2)
  ( 4 "Bip01 L Foot" 3)
  ( 5 "Bip01 R Thigh" 1)
  ( 6 "Bip01 R Calf" 5)
  ( 7 "Bip01 R Foot" 6)
  ( 8 "Bip01 Spine" 1)
  ( 9 "Bip01 Spine1" 8)
  (10 "Bip01 Neck" 9)
  (11 "Bip01 L Clavicle" 10)
  (12 "Bip01 L UpperArm" 11)
  (13 "Bip01 L Forearm" 12)
  (14 "Bip01 L Hand" 13)
  (15 "Bip01 L Finger0" 14)
  (16 "Bip01 L Finger01" 15)
  (17 "Bip01 L Finger02" 16)
  (18 "Bip01 L Finger1" 14)
  (19 "Bip01 L Finger11" 18)
  (20 "Bip01 L Finger12" 19)
  (21 "Bip01 L Finger2" 14)
  (22 "Bip01 L Finger21" 21)
  (23 "Bip01 L Finger22" 22)
  (24 "Bip01 L Finger4" 14)
  (25 "Bip01 L Finger41" 24)
  (26 "Bip01 L Finger42" 25)
  (27 "Bip01 R Clavicle" 10)
  (28 "Bip01 R UpperArm" 27)
  (29 "Bip01 R Forearm" 28)
  (30 "Bip01 R Hand" 29)
  (31 "Bip01 R Finger0" 30)
  (32 "Bip01 R Finger01" 31)
  (33 "Bip01 R Finger02" 32)
  (34 "Bip01 R Finger1" 30)
  (35 "Bip01 R Finger11" 34)
  (36 "Bip01 R Finger12" 35)
  (37 "Bip01 R Finger2" 30)
  (38 "Bip01 R Finger21" 37)
  (39 "Bip01 R Finger22" 38)
  (40 "Bip01 R Finger4" 30)
  (41 "Bip01 R Finger41" 40)
  (42 "Bip01 R Finger42" 41)
  (43 "Bip01 Head" 10)
  (44 "Bone05" 43)
)))

(define skeleton-pose (list->ff '(
      ; поворот                      ; смещение
  ( 0 (-0.001317 0.242067 39.204914) ( 0.000000 0.000000 -1.570795))
  ( 1 (-0.245774 0.000000 0.000000)  (-1.570795 -1.570451 0.000000))
  ( 2 (-0.000006 0.000008 4.427393)  (3.141292 -0.011240 3.068913))
  ( 3 (18.465940 0.000000 0.000000)  (0.000000 0.000000 -0.242123))
  ( 4 (14.875443 0.000000 -0.000001) (0.001118 -0.011190 0.169435))
  ( 5 (0.000006 -0.000004 -4.427393) (-3.141290 0.011243 3.068913))
  ( 6 (18.465940 0.000000 0.000000)  (0.000000 0.000000 -0.242123))
  ( 7 (14.875443 0.000000 0.000000)  (-0.001117 0.011190 0.169435))
  ( 8 (2.255207 -0.007574 0.000003)  (-0.000004 -0.000001 0.000796))
  ( 9 (9.514226 -0.009357 0.000000)  (0.000000 0.000000 0.000000))
  (10 (11.751820 -0.001633 0.000000) (0.000000 -0.000001 0.239386))
  (11 (-2.133122 0.444674 2.649729)  (0.088019 -1.370041 2.815150))
  (12 (4.640942  0.000000 -0.000001) (-0.049954 1.052208 -0.032367))
  (13 (10.828047 0.000000 -0.000002) (0.000000 0.000000 -0.409440))
  (14 (9.393336 -0.000001 0.000000)  (-1.569611 0.000000 0.165806))
  (15 (1.442245  0.107453 -1.746177) (1.585944 0.550080 0.132382))
  (16 (1.063452 -0.000001 -0.000001) (0.000000 0.000000 0.183260))
  (17 (1.376307 -0.000001 0.000000)  (0.000000 0.000000 0.148353))
  (18 (4.172847 -0.251366 -1.312744) (-0.001190 0.096055 0.052246))
  (19 (1.215373  0.000001 0.000000) (0.000000 0.000000 0.052360))
  (20 (1.215371 -0.000001 0.000000) (0.000000 0.000000 0.052360))
  (21 (4.220535 -0.244855 -0.431465) (-0.001184 0.026242 0.052329))
  (22 (1.350416  0.000000 0.000000) (0.000000 0.000000 0.052360))
  (23 (1.350410 -0.000001 -0.000001) (0.000000 0.000000 0.052360))
  (24 (4.262829 -0.232721 1.392178) (-0.001188 -0.078478 0.052453))
  (25 (1.012811  0.000000 0.000000) (0.000000 0.000000 0.052360))
  (26 (1.012809 -0.000001 0.000000) (0.000000 0.000000 0.052360))
  (27 (-2.133122 0.444686 -2.649726) (-0.088046 1.370039 2.815124))
  (28 (4.640942  0.000000 0.000000) (0.049954 -1.052208 -0.032367))
  (29 (10.828047 0.000000 0.000000) (0.000000 0.000000 -0.409440))
  (30 (9.393338  0.000000 0.000001) (1.569611 0.000000 0.165806))
  (31 (1.442242  0.107453 1.746178) (-1.585943 -0.550079 0.132381))
  (32 (1.063451  0.000000 -0.000001) (0.000000 0.000000 0.183260))
  (33 (1.376306 -0.000001 0.000000) (0.000000 0.000000 0.148353))
  (34 (4.172844 -0.251366 1.312745) (0.001190 -0.096055 0.052246))
  (35 (1.215373  0.000000 0.000001) (0.000000 0.000000 0.052360))
  (36 (1.215372  0.000000 0.000001) (0.000000 0.000000 0.052360))
  (37 (4.220531 -0.244854 0.431466) (0.001184 -0.026242 0.052329))
  (38 (1.350417  0.000001 -0.000001) (0.000000 0.000000 0.052360))
  (39 (1.350410 -0.000001 0.000001) (0.000000 0.000000 0.052360))
  (40 (4.262825 -0.232720 -1.392178) (0.001188 0.078478 0.052453))
  (41 (1.012810  0.000001 0.000000) (0.000000 0.000000 0.052360))
  (42 (1.012810 -0.000001 0.000000) (0.000000 0.000000 0.052360))
  (43 (2.110540  0.000000 0.000000) (0.000000 0.000000 -0.083184))
  (44 (0.891927  2.373051 -0.014527) (-3.141592 -0.000001 1.413797))
)))

(define skeleton-triangles (list->ff '(
  ("zombie_legs.bmp"
    ; кость   x-y-z                          normal                        uv
    (43     (-3.061285 -1.504443 68.762825) (-0.271391 0.752176  0.600481) (0.382813 0.843137))
    (43     (-2.821282  0.560357 68.958794) (-0.914014 0.395206 -0.091601) (0.425781 0.850980))
    (43     (-3.061287 -1.530083 67.584251) (-0.252096 0.872387 -0.418794) (0.378906 0.788235))
;zombie_legs.bmp
; 43 -3.061287 -1.530083 67.584251 -0.252096 0.872387 -0.418794 0.378906 0.788235
; 43 -2.821282 0.560357 68.958794 -0.914014 0.395206 -0.091601 0.425781 0.850980
; 43 -2.501285 0.402999 67.334061 -0.806372 0.559481 -0.191693 0.429688 0.784314

))))

;(define skeleton (list->ff (list
;  (
;  ; кости
;  (a . 1) (b . 2))))

(write (getf skeleton-bones 2))

;;;; =========================================================================================
(import (owl pinvoke))
(import (lib windows))
(import (OpenGL version-1-1))
(define OR (lambda list (fold bor 0 list)))

(define width 1280)
(define height 720)
(define window (CreateWindowEx
    (OR WS_EX_APPWINDOW WS_EX_WINDOWEDGE) "#32770" "Sceletal animation" ; #32770 is for system classname for DIALOG
    (OR WS_OVERLAPPEDWINDOW WS_CLIPSIBLINGS WS_CLIPCHILDREN)
    0 0 width height ; x y width height
    0 ; no parent window
    0 ; no menu
    0 ; instance
    null)) ; todo: override as '(INTEGER . 0)
(define pfd (list->byte-vector '(#x28 00  1  00  #x25 00 00 00 00 #x10 00 00 00 00 00 00
                                                   00 00 00 00 00 00 00 #x10 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00)))                        
(define hDC (GetDC window))
(define PixelFormat (ChoosePixelFormat hDC pfd))
(print "PixelFormat = " PixelFormat)
(print "SetPixelFormat = "
(SetPixelFormat hDC PixelFormat pfd))
(define hRC (wglCreateContext hDC))

(print "wglMakeCurrent = "
(wglMakeCurrent hDC hRC))

(print "OpenGL version: " (glGetString GL_VERSION))
(print "OpenGL vendor: " (glGetString GL_VENDOR)) 
(print "OpenGL renderer: " (glGetString GL_RENDERER))

(ShowWindow window SW_SHOW)
(SetForegroundWindow window)
(SetFocus window)

; ResizeGLScene
(glViewport 0 0 width height)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)

(glMatrixMode GL_MODELVIEW)
(glLoadIdentity)

(glShadeModel GL_SMOOTH)
(print "glHint = "
(glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST))

(glClearColor 0 0 1 1)

(define MSG "1234567890123456789012345678") ; sizeof(MSG)=28
;(call/cc (lambda (return)
(define (cycle)   ;MSG
  (if (= 1 (PeekMessage MSG 0 0 0 PM_REMOVE))
    (begin
      (TranslateMessage MSG)
      (DispatchMessage MSG))
    (begin ; DrawGLScene
      (glClear GL_COLOR_BUFFER_BIT)
       
      (glBegin GL_TRIANGLE_STRIP)
        (glVertex2f -1 -1)
        (glVertex2f +1 -1)
        (glVertex2f -1 +1)
        (glVertex2f +1 +1)
      (glEnd)
      (SwapBuffers hDC)))
  (if (= (GetAsyncKeyState 27) 0) (cycle)))
(cycle)

; KillGLWindow
(wglMakeCurrent 0 0)
(wglDeleteContext hRC)
(ReleaseDC window hDC)
(DestroyWindow window)

(halt 0)

#!/usr/bin/env ol

(import (lib gl-2)
   (scheme inexact))
(import (lib soil))
;(gl:set-window-size 900 900)

; menu
(import (olite menu))
(import (scheme inexact))
(import (olite vector))

;; ; ---- OLD CODE -----------
; init

; для VR кулинг будет другой
(define RL_CULL_DISTANCE_NEAR 0.10)
(define RL_CULL_DISTANCE_FAR 14000)


;; ; --------------------------------------------------------------------------------------------------
;; ; We should use one Uber Shader, because Oculus Go bugged (swithing program fails rendering process)
;; (setq STAGE-STARDUST 1)
;; (setq STAGE-TEXTURED 2)

(define colored-program (gl:create-program
"//#version 120 // OpenGL 2.1

void main()
{
   gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
   gl_FrontColor = vec4(0.0, 1.0, 0.0, 1.0);
   gl_BackColor = vec4(1.0, 0.0, 0.0, 1.0);
   gl_PointSize = 3.0;
}"

"//#version 120 // OpenGL 2.1
void main()
{
   gl_FragColor = gl_Color;
}"))

(define textured-program (gl:create-program
"

void main()
{
   gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;
   gl_TexCoord[0] = gl_MultiTexCoord0;
}"

"
uniform sampler2D tex0;
void main()
{
   gl_FragColor = texture2D(tex0, gl_TexCoord[0].st);
}"))

; --------------------------------
; TEMP
(import (scheme dynamic-bindings))
(define position (make-parameter '(0 0 5)))
(define target++ '( 0  0 -1)) ; вперед
(define up       '( 0  1  0))
(define speed '(#i0 #i0 #i0))

(define matrix [
   #i1 #i0 #i0 #i0
   #i0 #i1 #i0 #i0
   #i0 #i0 #i1 #i0
   #i0 #i0 #i0 #i1
])

(import (scheme inexact))
(import (scheme srfi-27))

; --------------------
(import (prefix (olite skybox) skybox:))
(import (prefix (olite stardust) stardust:))

(stardust:generate! (list->vector (position)))
(skybox:generate! 800 50)

; ....

; calc
(gl:set-calculator (lambda ()
   ; один раз отрисуем меню в текстуру
   ; да и вообще, здесь можно рендерить то, что нам надо только в плоском варианте (например, зеркало заднего вида)
   ;; (render-menu)

   (define time (/ (mod (time-ms) 62830) #i10000))
   (define s (/ (sin (* time 0.1)) 20))
   (define c (/ (cos (* time 0.1)) 20))

   ;(vm:set! (lref speed 2) 0 (* s 6) 0 (size s))

   (define newpos (map + (position) speed))
   (position newpos)
   (stardust:update! (list->vector newpos))

   #true
))

(define planet-program (gl:create-program
; придется в качестве мировой матрицы использовать gl_TextureMatrix
; ну не хочется свои функции работы с матрицами городить...
"  #version 120 // OpenGL 2.1
// разделяем видовую и модельную матрицы
#define my_ModelMatrix gl_TextureMatrix[0]

varying vec4 vertexPosition;
varying vec3 vertexNormal;
void main()
{
   // наша сфера не сферична (чтобы быстрее считать)
   // поэтому мы делаем ее сферичной тут:
   vec4 vertex = vec4(normalize(gl_Vertex.xyz), 1.0);
   // ну и заодно нормаль (в нашем случае совпадает)
   vec3 normal = vertex.xyz;

   vertexPosition = my_ModelMatrix * vertex;
   // нормаль тоже трансформируем в модельном пространстве (но не пространстве вида)
   vertexNormal = mat3(my_ModelMatrix) * normal;

   gl_Position = gl_ModelViewProjectionMatrix * vertexPosition;
   //gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * vertexPosition;
   gl_TexCoord[0] = vertex; // положение вершины в базовом пространстве (для текстурирования)
}"

"  #version 120 // OpenGL 2.1
#define PI 3.1415927410125732421875 // IEEE754 Pi Approximation

uniform sampler2D tex0;
uniform vec4 lightPosition;

varying vec4 vertexPosition;
varying vec3 vertexNormal;

void main()
{
   float x = gl_TexCoord[0].x,
         y = gl_TexCoord[0].y,
         z = gl_TexCoord[0].z;
   float phi = 1.0 - (1.0 + atan(x, -z)/PI) / 2.0;
   float theta = acos(y) / PI;

   vec3 vertex = vertexPosition.xyz;
   vec3 normal = normalize(vertexNormal);

   vec4 lightPos = lightPosition;
   vec3 lightDir = lightPos.xyz - vertex * lightPos.w;
   vec3 unitLightDir = normalize(lightDir);

   float diff = max(0.1, dot(normal, unitLightDir));
   //float diff = smoothstep(0.1, 1.0, dot(normal, unitLightDir));

   gl_FragColor = texture2D(tex0, vec2(phi, theta)) * diff;
   //gl_FragColor = vec4(normal.x, normal.y, normal.z, 1.0);

   //gl_FragColor = vec4(diff, 0, 0, 1.0);
}"))
(define mercury (let ((buffer (file->bytevector "media/solarsystemscope/2k_mercury.jpg")))
      (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGB SOIL_CREATE_NEW_ID 0))) ; SOIL_FLAG_MIPMAPS

(glBindTexture GL_TEXTURE_2D mercury)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glBindTexture GL_TEXTURE_2D 0)

(define geoid (glGenLists 1))
(glNewList geoid GL_COMPILE)
(begin
   (glBegin GL_TRIANGLES)

   ; если не нормализовать точки - страдает разбиение
   ; (?) нам не надо нормализовать точки, этим спокойно займется сам шейдер
   (define (loop a b c n)
      (vector-apply a (lambda (ax ay az)
      (vector-apply b (lambda (bx by bz)
      (vector-apply c (lambda (cx cy cz)
         (if (> n 0)
         then
            (define d (normalize [(/ (+ ax bx) 2) (/ (+ ay by) 2) (/ (+ az bz) 2)]))
            (define e (normalize [(/ (+ bx cx) 2) (/ (+ by cy) 2) (/ (+ bz cz) 2)]))
            (define f (normalize [(/ (+ cx ax) 2) (/ (+ cy ay) 2) (/ (+ cz az) 2)]))

            (loop a d f (-- n))
            (loop d b e (-- n))
            (loop e c f (-- n))
            (loop d e f (-- n))
         else
            (glVertex3fv a)
            (glVertex3fv b)
            (glVertex3fv c)) )))))))

   ; сгенерируем нужной точности сферу
   (define DIVISIONS 5) ; TODO: move to json config
   ; "нулевой" тетраэдр
   (define r (/ (sqrt 6) 4))
   (define m (negate r))
   (define vertices [
      [m m m] [r m r] [m r r] [r r m] ])
   ; генератор
   (for-each (lambda (a b c)
         (loop (normalize (ref vertices a))
               (normalize (ref vertices b))
               (normalize (ref vertices c)) DIVISIONS))
      '(1 2 3 4)
      '(2 3 4 1)
      '(3 4 1 2))
(glEnd))
(glEndList)


; draw
(import (lib GLU))
(gl:set-renderer (lambda ()
   (glViewport 0 0 (gl:get-window-width) (gl:get-window-height))
   (glClearColor 0 0 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; матрицы вида/проецирования, TODO: поворачивать мышкой
   ;; (unless vrapi
   (define FOVY 45.0)
   (define ASPECT (/ (gl:get-window-width) (gl:get-window-height)))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective FOVY ASPECT RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (apply gluLookAt (append
      (position) (map + (position) target++) up))

   ;; 1. Скайбокс
   (skybox:draw (list->vector (position)))
   (stardust:draw (list->vector speed))

   ;; 2. Планеты
   (glDisable GL_BLEND)
   (glEnable GL_DEPTH_TEST)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt
      0 0 9 ; position
      0 0 0 ; target 
      0 1 0)

   ; дальнейшие операции с миром мы проводим в модельном режиме
   ;  (за который мы принимаем текстурную матрицу)
   (glMatrixMode GL_TEXTURE)
   (glActiveTexture GL_TEXTURE0)

   ; temp: добавим на сцену свет
   ; направленный свет (w=0)
   (define lightPos '(400 0 0  1.0))
   ; а если точечный, то w=1

   ; подготовим модельную матрицу
   (define degrees (/ (mod (time-ms) 360000) 1000))
   (glLoadIdentity)
   (glTranslatef 0 0 0)
   (glScalef 3 3 3)
   (glRotatef (* degrees -10) 0 1 0)

   ; планета
   (glUseProgram planet-program)
   (glColor3f 1 1 1)
   (glActiveTexture GL_TEXTURE0)
   (glUniform1i (glGetUniformLocation planet-program "tex0") 0)
   (glUniform4fv (glGetUniformLocation planet-program "lightPosition") 1 lightPos)
   (glBindTexture GL_TEXTURE_2D mercury)
   (glCallList geoid)

   (glMatrixMode GL_TEXTURE)
   (glLoadIdentity)
   (apply glTranslatef lightPos)
   (glCallList geoid)

   ;; (glCallList geoid)

   ; солнце же прибьем гвоздями.


   ;; 3. Кораблики

   ;; (glUseProgram colored-program)
   ;; (glCallList twos)

   ;; ;; выведем текст меню в отдельное окошко
   ;; (glUseProgram textured-program)

   ;; (glActiveTexture GL_TEXTURE0)
   ;; (glUniform1i (glGetUniformLocation textured-program "tex0") 0)
   ;; (glBindTexture GL_TEXTURE_2D (get-menu-texture))

   ;; (glEnable GL_BLEND)
   ;; (glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)
   ;; (glEnable GL_DEPTH_TEST)

   ;; (glBegin GL_QUADS)
   ;;    (glTexCoord2f 0 0)
   ;;    (glVertex3f -3 +3 -2)
   ;;    (glTexCoord2f 1 0)
   ;;    (glVertex3f +3 +3 -2)
   ;;    (glTexCoord2f 1 1)
   ;;    (glVertex3f +3 -3 -2)
   ;;    (glTexCoord2f 0 1)
   ;;    (glVertex3f -3 -3 -2)
   ;; (glEnd)
   #true
))

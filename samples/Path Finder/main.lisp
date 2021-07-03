#!/usr/bin/env ol

(define show-path #f)

;;;; Алгоритм ориентирования и поиска пути в сложном лабиринте
(import
   (owl ff) (otus random!)
   (scheme dynamic-bindings)
   (lib gl))

; ===========================================================================
(define scheme [
   [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  1 1 1 1 1 1 1 2 2 1 1 1 1 1]
   [1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 1 0 0 0 0 2 2 0 0 0 0 1]
   [1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 1 1 0 0 0 2 2 0 1 0 0 1]
   [1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1  0 0 0 0 2 2 0 0 2 2 0 0 0 1]
   [1 0 1 1 1 1 1 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 3 0 2 2 0 2 2 0 0 0 1]
   [1 0 1 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 0 1 1 1 0 1 1  0 0 0 0 0 0 2 2 2 2 0 1 0 1]
   [1 0 1 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 3 0 0 2 2 0 0 0 0 1]
   [1 0 1 0 0 0 1 0 0 0 0 1 1 0 1 1 1 1 0 1 1 0 0 0 0 1  0 3 0 0 0 0 0 2 2 2 0 0 0 1]
   [1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 0 0 0 0 0 2 2 2 0 0 0 1]
   [1 0 1 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 3 0 0 0 0 2 2 0 0 0 0 1]
   [1 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 0 0 0 0 0 2 2 0 0 0 0 1]
   [1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1  0 0 0 3 0 0 0 2 2 0 0 0 0 1]
   [1 0 1 0 0 0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 1 0 0 0 0 2 2 2 0 0 0 0 1]
   [1 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 2 2 2 0 0 0 0 0 1]
   [1 0 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 2 2 0 0 0 0 3 0 1]
   [1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 2 2 0 3 0 0 0 0 1]
   [1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 0 0 2 2 0 0 0 0 0 0 0 1]
   [1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 0 0 0 0 0 3 0 0 1]
   [1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 2 2 0 0 0 0 0 0 0 1]
   [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  1 1 1 1 2 2 1 1 1 1 1 1 1 1]])

(define (at x y)
   (ref (ref scheme y) x))

; константы
(define WIDTH (size (ref scheme 1)))
(define -WIDTH (- WIDTH))
(define +WIDTH (+ WIDTH))
(define HEIGHT (size scheme))
(define -HEIGHT (- HEIGHT))
(define +HEIGHT (+ HEIGHT))

(print "WIDTH: " WIDTH)
(print "HEIGHT: " HEIGHT)

(define WIDTH-1 (- WIDTH 1))
(define HEIGHT-1 (- HEIGHT 1))
(define WIDTH+1 (+ WIDTH 1))
(define HEIGHT+1 (+ HEIGHT 1))

; загрузка модуля с ai
(import (ai))
(print "ai loaded")

(define hero (new-creature))
(mail hero ['set-location (cons 2 2)])
(mail hero ['look-around scheme])

(gl:set-window-title "Pathfinder sample")
(import (OpenGL version-1-1))
(import (OpenGL EXT bgra))


(define (quad x y)
   (glVertex2f x y)
   (glVertex2f x (+ y 1))
   (glVertex2f (+ x 1) (+ y 1))
   (glVertex2f (+ x 1) y))

(define (quadT x y u v)
   (glTexCoord2f    u         v)
   (glVertex2f x y)
   (glTexCoord2f    u      (+ v 1/8))
   (glVertex2f x (+ y 1))
   (glTexCoord2f (+ u 1/8) (+ v 1/8))
   (glVertex2f (+ x 1) (+ y 1))
   (glTexCoord2f (+ u 1/8)    v)
   (glVertex2f (+ x 1) y))


(define cell-codes '(
   ((#T #T #T #T) . (3/8 . 3/8)) ; монолитная стена

   ; прямые стены:
   ((#F #F #T #T) . (3/8 . 1/8)) ; сверху вниз
   ((#T #T #F #F) . (1/8 . 3/8)) ; слева направо

   ; углы:
   ((#T #F #F #T) . (4/8 . 2/8)) ; правый-верхний угол
   ((#T #F #T #F) . (4/8 . 4/8)) ; правый-нижний угол
   ((#F #T #F #T) . (2/8 . 2/8)) ; левый-верхний угол
   ((#F #T #T #F) . (2/8 . 4/8)) ; левый-нижний угол

   ; ответвления:
   ((#T #F #T #T) . (2/8 . 3/8)) ; влево
   ((#T #T #F #T) . (3/8 . 4/8)) ; вниз
   ((#T #T #T #F) . (3/8 . 2/8)) ; вверх
   ((#F #T #T #T) . (4/8 . 3/8)) ; вправо

   ; тупики:
   ((#T #F #F #F) . (6/8 . 3/8)) ; правый тупик
   ((#F #T #F #F) . (0/8 . 3/8)) ; левый тупик
   ((#F #F #T #F) . (3/8 . 6/8)) ; нижний тупик
   ((#F #F #F #T) . (3/8 . 0/8)) ; верхний тупик

   ; островок
   ((#F #F #F #F) . (3/8 . 3/8)) ))

(define (draw-map-cell x y cell alpha)
   (cond
   ((eq? cell 0) ; пустой пол
      (glColor3f alpha alpha alpha)
      (quadT x y 1/8 1/8))
   ((eq? cell 2) ; вода
      (glColor3f 0 (* alpha 0.4) alpha)
      (quadT x y 6/8 6/8))
   ((eq? cell 3) ; кусты
      (glColor3f 0 alpha (* alpha 0.4))
      (quadT x y 5/8 5/8))
   ((eq? cell 1)
      (glColor3f alpha alpha alpha)
      (define code (list
         (eq? (at (- x 1) y) 1)
         (eq? (at (+ x 1) y) 1)
         (eq? (at x (- y 1)) 1)
         (eq? (at x (+ y 1)) 1)))

      (call/cc (lambda (break)
         (for-each (lambda (p)
               (when (equal? (car p) code)
                  (quadT x y (cadr p) (cddr p))
                  (break #t)))
            cell-codes)))) ))

; окно - рисовалка
; ---------------------------------------------------------------------------

; init
(glMatrixMode GL_PROJECTION)
(glLoadIdentity) ; тут надо зеркально отразить карту сверху вниз
(glOrtho -1 (+ WIDTH 3) (+ HEIGHT 3) -1  -1 1)
(glMatrixMode GL_MODELVIEW)

(glEnable GL_TEXTURE_2D)
(glBindTexture GL_TEXTURE_2D 1)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
(glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
(glTexImage2D GL_TEXTURE_2D 0 GL_RGB8
   128 128
   0 GL_BGR GL_UNSIGNED_BYTE (file->bytevector "tileset.rgb"))

(define (find-empty-random-place)
   (let do ((x (rand! WIDTH))
            (y (rand! HEIGHT)))
      (if (eq? (at x y) 0)
         (cons x y)
         (do (rand! WIDTH) (rand! HEIGHT)))))

; dynamic userdata
(define userdata
   (let ((xy (find-empty-random-place)))
      (make-parameter {
         'x (car xy)
         'y (cdr xy)
         'old-time (let* ((new-time _ (clock))) new-time)
      })))

; draw
(gl:set-renderer (lambda (mouse)
(let*((ud (userdata))
      (x (ud 'x)) ; позиция яблокаs
      (y (ud 'y))
      (old-time (ud 'old-time)))

   (glClearColor 0.2 0.2 0.2 1.)
   (glClear GL_COLOR_BUFFER_BIT)

   ; нарисуем карту как она есть
   ; для пользователя (всю карту видим только мы)
   ;; (glEnable GL_TEXTURE_2D)
   ;; (glBindTexture GL_TEXTURE_2D 1)
   ;; (glBegin GL_QUADS)
   ;;    (for-each (lambda (y)
   ;;          (for-each (lambda (x)
   ;;                (draw-map-cell x y (at x y) 0.2))
   ;;             (iota WIDTH 1)))
   ;;       (iota HEIGHT 1))
   ;; (glEnd)
   ;; (glDisable GL_TEXTURE_2D)

   ; ходим не чаше ? раз в секунду
   (let*((new-time _ (clock))
         (step (interact hero ['A* x y])))

      ; нарисуем карту, которую "помнит" создание
      (if #t
      (let ((map (interact hero ['get-fov])))
         (glEnable GL_TEXTURE_2D)
         (glBegin GL_QUADS)
         (ff-fold (lambda (_ key value)
               (let* ((x y (unhash key)))
                  (draw-map-cell x y (car value) (/ (cdr value) 600))
               #t))
            #f map)
         (glEnd))
      )

      ; дебаг ИИ
      (let ((map (ref step 3)))
         ; draw 'open list
         (glDisable GL_TEXTURE_2D)
         (glBegin GL_LINES)
         (ff-fold (lambda (n v xyp)
                     (let ((from (ref xyp 1))
                           (to   (ref xyp 2)))
                        (if to
                           (begin
                              (glColor3f 1.0 1.0 1.0)
                              (glVertex2f (+ (car from) 0.5) (+ (cdr from) 0.5))
                              (glColor3f 0.2 0.2 0.2)
                              (glVertex2f (+ (car to) 0.5) (+ (cdr to) 0.5))))))
            #f map)
         (glEnd))


      ; нарисуем, где сейчас находится наш герой:
      (glEnable GL_TEXTURE_2D)
      (glColor3f 1 1 1)
      (glBegin GL_QUADS)
      (let ((xy (interact hero ['get-location])))
         (if (and (= (car xy) x) (= (cdr xy) y))
            (quadT x y 7/8 1/8)
            (begin
               (quadT (car xy) (cdr xy) 5/8 1/8) ; где сейчас
               (quadT x y 6/8 1/8)))) ; куда собирается пойти
      (glEnd)

      ;; ; отрисуем точки на карте, ибо хочу посмотреть
      ;; (glDisable GL_TEXTURE_2D)
      ;; (glColor3f 1 0 0)
      ;; (glBegin GL_POINTS)
      ;;    (for-each (lambda (x)
      ;;          (for-each (lambda (y)
      ;;                (glVertex2f x y))
      ;;             (iota HEIGHT 1)))
      ;;       (iota WIDTH 1))
      ;; (glEnd)

      ; все, теперь можем двигаться туда,  куда хотели:
      ;; (when (not (eq? new-time old-time))
      (mail hero ['move (ref step 1) (ref step 2)])
      (mail hero ['look-around scheme]) ;)

      ; вернем модифицированные параметры
      (userdata (put
         (if (and (eq? (ref step 1) 0) (eq? (ref step 2) 0) (> new-time old-time))
            (let do ((x (rand! WIDTH))
                     (y (rand! HEIGHT)))
               (if (eq? (at x y) 0)
                  (put (put ud 'x x) 'y y)
                  (do (rand! WIDTH) (rand! HEIGHT))))
         else
            ud)
         ; send new
         'old-time new-time))))))


;(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
;(call/cc (lambda (return)
;   ;else
;   (case key
;      (32
;         (mail me (tuple 'update-fov scheme)))
;
;      (39
;         (mail me (tuple 'move +1 0))
;         (mail me (tuple 'update-fov scheme)))
;      (37
;         (mail me (tuple 'move -1 0))
;         (mail me (tuple 'update-fov scheme)))
;
;      ; вверх
;      (38
;         (mail me (tuple 'move 0 -1))
;         (mail me (tuple 'update-fov scheme)))
;      ; вниз
;      (40
;         (mail me (tuple 'move 0 +1))
;         (mail me (tuple 'update-fov scheme))))
;
;   userdata)))))
;
;(mail 'opengl (tuple 'set-mouse (lambda (userdata  lbutton rbutton x y)
;(call/cc (lambda (return)
;   (if lbutton (begin
;      (mail me (tuple 'set-location (to-map-from-screen (cons x y))))
;      (mail me (tuple 'update-fov scheme))
;      (return (put userdata 'mouse (to-map-from-screen (cons x y))))))
;   userdata)))))

(gl:set-mouse-handler (lambda (button x y)
   (when (eq? button 1)
      (define px (floor (- (* (/ x (ref gl:window-dimensions 3)) (+ WIDTH 4)) 1)))
      (define py (floor (- (* (/ y (ref gl:window-dimensions 4)) (+ HEIGHT 4)) 1)))

      (if (eq? (at px py) 0)
         (userdata 
            (put (put (userdata) 'x px) 'y py))) )))

         ;; (let do ((x (rand! WIDTH))
         ;;          (y (rand! HEIGHT)))
         ;;    (if (eq? (at x y) 0)
         ;;       (put (put (userdata) 'x x) 'y y)
         ;;       (do (rand! WIDTH) (rand! HEIGHT))))))))

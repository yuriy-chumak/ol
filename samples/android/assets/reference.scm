#!/usr/bin/ol

; основнi показники персонажа
(define стать '(0 . 2))
(define раса '(0 . 11))
(define клас '(0 . 6))

(define сила '(0))
(define інтелект '(0))
(define мудрість '(0))
(define спритність '(0))
(define статура '(0))
(define чарівність '(0))

,load "характеристики.lisp"

; -----------------------------------------------------------------
; зададим конфигурацию графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff `(
      ; размеры окна в знакоместах:
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width . ,(* 80 9))
      (height . ,(* 16 25)))))))

(import (lib gl))
(import (lib gl console))
(import (otus random!))

; Cross-OS trick (use different opengl's for different OSes):
(define-library (lib gl common)
   (import (scheme core))
   (export version)
   (cond-expand
      (Android
         (import (OpenGL ES version-1-1))
         (export (exports (OpenGL ES version-1-1))))
      (else
         (import (OpenGL version-1-1))
         (export (exports (OpenGL version-1-1)))))
   (begin (define version "1.0")))
(import (lib gl common))

; рендерер:
(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (render-windows)
))


; временное окно дебага (покажем fps):
(define fps (create-window 70 24 10 1))
(define started (time-ms)) (define time (list 0))
(define frames '(0 . 0))

(set-window-border fps YELLOW)
(set-window-writer fps (lambda (type)
   (set-car! frames (+ (car frames) 1))
   (let ((now (time-ms)))
      (if (> now (+ started (car time) 1000))
         (begin
            (set-cdr! frames (car frames))
            (set-car! frames 0)
            (set-car! time (- now started)))))
   (type WHITE (cdr frames) " fps")
))

(define запрошення (create-window 2 2 76 5))
(set-window-writer запрошення (lambda (write)
   (write CYAN  "Створіть героя за допомогою наступних меню:\n\n")
   (write WHITE "Використайте " CYAN "курсорні клавіші" WHITE " для прокрутки меню або обирайте потрiбне мишкою,\n")
   (write WHITE "'" CYAN "Enter" WHITE "' - для вибору, ")
   (write WHITE "'" CYAN "*" WHITE "' - для випадкового вибору, '" CYAN "Esc" WHITE "' - почати спочатку,\n")
   (write WHITE "'" CYAN "?" WHITE "' - довiдка, '" CYAN "Ctrl-x" WHITE "' - для виходу.")
))

(define підказка (create-window 2 9 76 1))
(define номер-підказки '(0 . 3))
(set-window-writer підказка (lambda (write)
   (write YELLOW)
   (case (car номер-підказки)
      (0
         (write "Ваша стать не має помітних ефектів у грі."))
      (1
         (write "Ваша раса визначає різноманітні можливості та бонуси."))
      (2
         (write "Ваш клас визначає різноманітні можливості та бонуси.\n"
                "Затемнені пункти рекомендуються тільки досвідченим гравцям")))
))

(define вибір-статі (create-window 2 12 11 2))
(set-window-writer вибір-статі (lambda (write)
   (if (= (car стать) 0)
      (write CYAN)
      (write WHITE))
   (write 'жіноча "а) жіноча\n" #f)
   (if (= (car стать) 1)
      (write CYAN)
      (write WHITE))
   (write 'чоловіча "б) чоловіча" #f)
))

(define вибір-раси (create-window 16 12 15 11))
(set-window-writer вибір-раси (lambda (write)
   (define (C n)
      (if (= (car раса) n)
         (write CYAN)
         (write WHITE)))
   (C 0) (write 'людина "а) Людина\n" #f)
   (C 1) (write 'напівельф "b) Напівельф\n" #f)
   (C 2) (write 'ельф "c) Ельф\n" #f)
   (C 3) (write 'хоббіт "d) Хоббіт\n" #f)
   (C 4) (write 'карлик "e) Карлик\n" #f)
   (C 5) (write 'гном "f) Гном\n" #f)
   (C 6) (write 'напіворк "g) Напіворк\n" #f)
   (C 7) (write 'напівтроль"h) Напівтроль\n" #f)
   (C 8) (write 'дунадан "i) Дунадан\n" #f)
   (C 9) (write 'високий-ельф "j) Високий Ельф\n" #f)
   (C 10) (write 'кобольд "k) Кобольд" #f)
))

(define вибір-класу (create-window 33 12 12 6))
(set-window-writer вибір-класу (lambda (write)
   (define (C n q)
      (if (= (car клас) n)
         (write (if (memq (car раса) q) BLUE CYAN))
         (write (if (memq (car раса) q) GRAY WHITE))))
   (C 0 '()) (write 'воїн "а) Воїн\n" #f)
   (C 1 '(3 5 6 7 10)) (write 'маг "b) Маг\n" #f)
   (C 2 '(2 3)) (write 'священник "c) Священник\n" #f)
   (C 3 '(5 7 8 9)) (write 'крадій "d) Крадій\n" #f)
   (C 4 '(4 5 6 7)) (write 'слідопит "e) Слідопит\n" #f)
   (C 5 '(2 3 4 6 7 9 10)) (write 'паладін "f) Паладін" #f)
))

(define показник (create-window 52 12 14 6))
(set-window-writer показник (lambda (write)
   (define (p x)
      (cond
         ((> x 0)
            (write "+"))
         ((= x 0)
            (write "+")))
      (write x))

   (write LIGHTGRAY)
   (write "сила: ") (p ((СИЛА (car раса)) (car клас))) (write "\n")
   (write "інтелект: ") (p ((ІНТЕЛЕКТ (car раса)) (car клас))) (write "\n")
   (write "мудрість: ") (p ((МУДРІСТЬ (car раса)) (car клас))) (write "\n")
   (write "спритність: ") (p ((СПРИТНІСТЬ (car раса)) (car клас))) (write "\n")
   (write "статура: ") (p ((СТАТУРА (car раса)) (car клас))) (write "\n")
   (write "чарівність: ") (p ((ЧАРІВНІСТЬ (car раса)) (car клас))) (write "\n")
   (write "\n")
   (write "Здоров'я: ") (p ((ЗДОРОВЬЕ (car раса)) (car клас))) (write "\n")
   (write "Опыт: ") (p ((ОПЫТ (car раса)) (car клас))) (write "%\n")
   (write "Инфразрение: ") (p ((ИНФРАЗРЕНИЕ (car раса)) (car клас))) (write " фт\n")
))

; ...
(define (задати-підказку n)
   (set-car! номер-підказки n)
   (set-window-border вибір-статі (if (eq? n 0) BLUE))
   (set-window-border вибір-раси (if (eq? n 1) BLUE))
   (set-window-border вибір-класу (if (eq? n 2) BLUE))
)
(задати-підказку 0)

; ...
; custom "message box" function:
(define (message-box title text options handler)
   ; for now - two buttons
   (define box (create-window 30 10 20 5))
   (define ok (create-window 33 13 4 1))
   (define cancel (create-window 39 13 8 1))
   (define selection '(0 . 2))

   (set-window-background box GRAY)
   (set-window-border box WHITE)

   (set-car! selection 0) ; todo: get "default button" from options
   (set-window-background ok RED)
   (set-window-background cancel GRAY)

   ; save old handlers
   (define old-mouse-handler (interact 'opengl (tuple 'get 'mouse-handler)))
   (define old-keyboard-handler (interact 'opengl (tuple 'get 'keyboard-handler)))

   (set-window-writer box (lambda (write)
      (write "\n  " text)
   ))
   (set-window-writer ok (lambda (write) (write " Ok ")))
   (set-window-writer cancel (lambda (write) (write " Cancel ")))

   (gl:set-keyboard-handler (lambda (key)
      (case key
         (vkLeft
            (set-car! selection (mod (+ (car selection) 1) 2))
            (set-window-background ok (if (zero? (car selection)) RED GRAY))
            (set-window-background cancel (if (zero? (car selection)) GRAY RED)))
         (vkRight
            (set-car! selection (mod (+ (car selection) 1) 2))
            (set-window-background ok (if (zero? (car selection)) RED GRAY))
            (set-window-background cancel (if (zero? (car selection)) GRAY RED)))
         (vkEnter
            (for-each destroy-window (list ok cancel box))
            (gl:set-keyboard-handler old-keyboard-handler)
            (gl:set-mouse-handler old-mouse-handler)

            (handler (zero? (car selection)))))))

   (gl:set-mouse-handler (lambda (button x y)
      #true))

   #true)

(gl:set-keyboard-handler (lambda (key)
   (case key
      (vkQ (halt vkQ))
      (vkEnter
         (mail 'game (tuple 'save)))
      (vkStar
         (set-car! клас (rand! (cdr клас)))
         (set-car! раса (rand! (cdr раса)))
         (set-car! стать (rand! (cdr стать))))
      (vkEsc
         (set-car! клас 0)
         (set-car! раса 0)
         (set-car! стать 0)
         (задати-підказку 0))

      (vkRight
         (задати-підказку (mod (+ (car номер-підказки) 1) (cdr номер-підказки))))
      (vkLeft
         (задати-підказку (mod (+ (car номер-підказки) (- (cdr номер-підказки) 1)) (cdr номер-підказки))))
      (vkDown
         (case (car номер-підказки)
            (0 (set-car! стать (mod (+ (car стать) 1) (cdr стать))))
            (1 (set-car! раса (mod (+ (car раса) 1) (cdr раса))))
            (2 (set-car! клас (mod (+ (car клас) 1) (cdr клас))))))
      (vkUp
         (case (car номер-підказки)
            (0 (set-car! стать (mod (+ (car стать) (- (cdr стать) 1)) (cdr стать))))
            (1 (set-car! раса (mod (+ (car раса) (- (cdr раса) 1)) (cdr раса))))
            (2 (set-car! клас (mod (+ (car клас) (- (cdr клас) 1)) (cdr клас))))))
   )))

(gl:set-mouse-handler (lambda (button x y)
   (задати-підказку
      (case (windows-make-selection x y)
         ('жіноча   (set-car! стать 0) 0)
         ('чоловіча (set-car! стать 1) 0)

         ('людина    (set-car! раса 0) 1)
         ('напівельф (set-car! раса 1) 1)
         ('ельф      (set-car! раса 2) 1)
         ('хоббіт    (set-car! раса 3) 1)
         ('карлик    (set-car! раса 4) 1)
         ('гном      (set-car! раса 5) 1)
         ('напіворк  (set-car! раса 6) 1)
         ('напівтроль(set-car! раса 7) 1)
         ('дунадан   (set-car! раса 8) 1)
         ('високий-ельф (set-car! раса 9) 1)
         ('кобольд   (set-car! раса 10) 1)

         ('воїн      (set-car! клас 0) 2)
         ('маг       (set-car! клас 1) 2)
         ('священник (set-car! клас 2) 2)
         ('крадій    (set-car! клас 3) 2)
         ('слідопит  (set-car! клас 4) 2)
         ('паладін   (set-car! клас 5) 2)
   ))
))

;; (gl:set-keyboard-handler (lambda (key)
;;    (define (incr x) (set-car! x (mod (+ (car x) +1) (cdr x))))
;;    (define (decr x) (set-car! x (mod (+ (car x) -1 (cdr x)) (cdr x))))

;;    (case key
;;       (vkQ (halt vkQ))
;;       (vkEsc (halt 0))

;;       (vkLeft
;;          (case (car selection)
;;             ((1 2 3)
;;                (decr selection)))
;;          (update-window-borders))
;;       (vkRight
;;          (case (car selection)
;;             ((0 1 2)
;;                (incr selection)))
;;          (update-window-borders))

;;       (vkUp
;;          (case (car selection)
;;             (0 (decr sex))
;;             (1 (decr race))
;;             (else
;;                (print "unknown selection " selection))))
;;       (vkDown
;;          (case (car selection)
;;             ; первая колонка?
;;             (0 (incr sex))
;;             (1 (incr race))
;;             ; третья колонка?
;;             ;((equal? selection '(3))
;;             (else
;;                (print "unknown selection " selection))))
;;       (vkEnter
;;          (case (car selection)
;;             ((0 1 2)
;;                (incr selection))
;;             (3
;;                (fasl-save `(
;;                   (sex . ,(car sex)) ; Пол персонажа,
;;                   (race . ,(car race)) ; Раса и
;;                   (class . ,(car class)) ; Класс

;;                   ; Показатели
;;                   (strength . ,(car strength)) ; Сила
;;                   (intelligence . ,(car intelligence)) ; Интеллект
;;                   (wisdom . ,(car wisdom)) ; Мудрость
;;                   (dexterity . ,(car dexterity)) ; Ловкость
;;                   (constitution . ,(car constitution)) ; Телосложение
;;                   (charm . ,(car charm)) ; Обаяние
;;                ) "hero.bin")
;;                (syscall 59 (c-string "/usr/bin/ol") (map c-string '("/usr/bin/ol" "main.lisp")) #false)
;;                (halt 0))))
;;       ;; (vkRight
;;       ;;    (set-car! selection (+ (car selection) 1)))
;;       (63 ; *
;;          (set-car! sex (rand! (cdr sex)))
;;          (set-car! race (rand! (cdr race)))
;;          (set-car! class (rand! (cdr class)))
;;          (set-car! selection 2))



;;       (else
;;          (print "unhandled key: " key)))))

;; (gl:set-mouse-handler (lambda (button x y)
;;    (let ((selection (windows-make-selection x y)))
;;       (cond
;;          ((function? selection)
;;             (selection))))))

;; ; вот тут ждем пока не будет сделан окончательный выбор
;; ;; (let loop ((* #f))
;; ;;    (unless (eq? (car selection) 4)
;; ;;       (loop (sleep 8))))
;; (print "ok.")

;; ; тут надо пересоздать окна и обновить обработчик клавиатуры
;; ; TBD.

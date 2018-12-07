#!/usr/bin/ol

; зададим конфигурацию графического окна
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff `(
      ; размеры окна в знакоместах
      ; напомню, что мы используем фиксированный шрифт размера 9*16
      (width . ,(* 80 9))
      (height . ,(* 16 25)))))))

; блок переменных - индикаторов выбора пользователя
(define selection '(0 . 4))

; пара (текущий-выбор . количество-вариантов)
(define sex '(0 . 2)) ; Пол персонажа,
(define race '(0 . 11)) ; Раса и
(define class '(0 . 6)) ; Класс

; Показатели
(define strength '(0)) ; Сила
(define intelligence '(0)) ; Интеллект
(define wisdom '(0)) ; Мудрость
(define dexterity '(0)) ; Ловкость
(define constitution '(0)) ; Телосложение
(define charm '(0)) ; Обаяние

; Навыки



; -------------------------------------------------------------------
; подключение графической библиотеки и библиотеки графической консоли
(import (lib gl))
(import (lib gl console))
(import (otus random!))

; Cross-os trick (use different opengl's for different OSes):
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

; временное окно дебага (покажем fps):
(define fps (create-window 70 24 10 1))
(define started (time-ms)) (define time (list 0))
(define frames '(0 . 0))

(set-window-writer fps (lambda (type)
   (set-car! frames (+ (car frames) 1))
   (let ((now (time-ms)))
      (if (> now (+ started (car time) 1000))
         (begin
            (set-cdr! frames (car frames))
            (set-car! frames 0)
            (set-car! time (- now started)))))
   (type GRAY (cdr frames) " fps")
))

; -----------------
; окно приветствия:
(define welcome (create-window 2 2 76 5))
(set-window-writer welcome (lambda (type)
   (type CYAN "Создайте героя с помощью следующих меню:" "\n\n")
   (type WHITE "Используйте " GREEN "курсорные клавиши" WHITE " для прокрутки меню, "
      GREEN "Enter" WHITE " для выбора, '" GREEN "*" WHITE "' для" "\n")
   (type WHITE "случайного выбора, '" GREEN "ESC" "' - начать сначала, '"
          GREEN "=" WHITE "' - опции рождения, '" GREEN "?" WHITE "' -" "\n")
   (type WHITE "справка, '" GREEN "Ctrl-X" WHITE "' - для выхода.")
))

; окно с комментариями:
(define comments (create-window 2 8 76 2))
(set-window-writer comments (lambda (type)
   (type YELLOW
      (case (car selection)
         (0 "Ваш пол не имеет заметных эффектов в игре.")
         (1 "Ваша раса определяет различные возможности и бонусы.")
         (2 "Ваш класс определяет ваши способности и бонусы.\nЗатемненные пункты рекомендуется только опытным игрокам.")
         (3 "Нажмите ENTER для выбора текущего параметров.")))
))

(define sex-selection (create-window 2 11 10 2))
(define race-selection (create-window 14 11 14 11))
(define class-selection (create-window 24 11 32 11))
(define make-a-choise (create-window 5 5 6 6))

(define (update-window-borders)
   (set-window-border sex-selection (if (eq? (car selection) 0) GRAY))
   (set-window-border race-selection (if (eq? (car selection) 1) GRAY))
   (set-window-border class-selection (if (eq? (car selection) 2) GRAY))
   (set-window-border make-a-choise (if (eq? (car selection) 3) GRAY)))
(update-window-borders)


; функция-помощник, создает функцию подсвечивания выбора и обработки клика мышкой
(define (make-printer sel var write)
   (lambda (x . args)
      (apply write (append (list
         (if (eq? (car var) x) CYAN WHITE) (lambda ()
                                             (set-car! selection sel) (set-car! var x)
                                             (update-window-borders)))
         args))
      (write "\n")))

; выбор пола персонажа
(set-window-writer sex-selection (lambda (write)
   (define print (make-printer 0 sex write))

   (print 0 "a) Женский")
   (print 1 "b) Мужской")
))

; выбор расы персонажа
(set-window-writer race-selection (lambda (write)
   (define print (make-printer 1 race write))
   (define (sign ab)
      (if (< ab 0) "-" "+"))
   (define str (car strength))
   (define int (car intelligence))
   (define wis (car wisdom))
   (define dex (car dexterity))
   (define con (car constitution))
   (define cha (car charm))

   (case (car selection)
      (0 #false) ; если выбирают персонажа, ничего не пишем
      (1
         (print 0 "a) Человек" #f "     Сил: " (sign str) str)
         (print 1 "b) Полуэльф" #f "    Инт: " (sign int) int)
         (print 2 "c) Эльф" #f "        Муд: " (sign wis) wis)
         (print 3 "d) Хоббит" #f "      Лов: " (sign dex) dex)
         (print 4 "e) Карлик" #f "      Тел: " (sign con) con)
         (print 5 "f) Гном" #f "        Оба: " (sign cha) cha)
         (print 6 "g) Полуорк" #f "     Здоровье   : " 10)
         (print 7 "h) Полутролль" #f "  Опыт       : " 100 "%")
         (print 8 "i) Дунадан" #f "     Инфразрение: " 0 " фт")
         (print 9 "j) Высший Эльф" #f)
         (print 10 "k) Кобольд" #f))
      ;; ((2 3 4) (apply type (list
      ;;       (color 0) "a) Человек" #f "     \n"
      ;;       (color 1) "b) Полуэльф" #f "    \n"
      ;;       (color 2) "c) Эльф" #f "        \n"
      ;;       (color 3) "d) Хоббит" #f "      \n"
      ;;       (color 4) "e) Карлик" #f "      \n"
      ;;       (color 5) "f) Гном" #f "        \n"
      ;;       (color 6) "g) Полуорк" #f "     \n"
      ;;       (color 7) "h) Полутролль" #f "  \n"
      ;;       (color 8) "i) Дунадан" #f "     \n"
      ;;       (color 9) "j) Высший Эльф" #f)))
   )
))

; рендерер:
(gl:set-renderer (lambda (mouse)
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (render-windows)
))

(gl:set-keyboard-handler (lambda (key)
   (define (incr x) (set-car! x (mod (+ (car x) +1) (cdr x))))
   (define (decr x) (set-car! x (mod (+ (car x) -1 (cdr x)) (cdr x))))

   (case key
      (vkQ (halt vkQ))
      (vkEsc (halt 0))

      (vkLeft
         (case (car selection)
            ((1 2 3)
               (decr selection)))
         (update-window-borders))
      (vkRight
         (case (car selection)
            ((0 1 2)
               (incr selection)))
         (update-window-borders))

      (vkUp
         (case (car selection)
            (0 (decr sex))
            (1 (decr race))
            (else
               (print "unknown selection " selection))))
      (vkDown
         (case (car selection)
            ; первая колонка?
            (0 (incr sex))
            (1 (incr race))
            ; третья колонка?
            ;((equal? selection '(3))
            (else
               (print "unknown selection " selection))))
      (vkEnter
         (case (car selection)
            ((0 1 2)
               (incr selection))
            (3
               (fasl-save `(
                  (sex . ,(car sex)) ; Пол персонажа,
                  (race . ,(car race)) ; Раса и
                  (class . ,(car class)) ; Класс

                  ; Показатели
                  (strength . ,(car strength)) ; Сила
                  (intelligence . ,(car intelligence)) ; Интеллект
                  (wisdom . ,(car wisdom)) ; Мудрость
                  (dexterity . ,(car dexterity)) ; Ловкость
                  (constitution . ,(car constitution)) ; Телосложение
                  (charm . ,(car charm)) ; Обаяние
               ) "hero.bin")
               (syscall 59 (c-string "/usr/bin/ol") (map c-string '("/usr/bin/ol" "main.lisp")) #false)
               (halt 0))))
      ;; (vkRight
      ;;    (set-car! selection (+ (car selection) 1)))
      (63 ; *
         (set-car! sex (rand! (cdr sex)))
         (set-car! race (rand! (cdr race)))
         (set-car! class (rand! (cdr class)))
         (set-car! selection 2))



      (else
         (print "unhandled key: " key)))))

(gl:set-mouse-handler (lambda (button x y)
   (let ((selection (windows-make-selection x y)))
      (cond
         ((symbol? selection)
            (case selection
               ('male
                  (print "male"))
               ('female
                  (print "female"))
            ))
         ((function? selection)
            (selection))))))

; вот тут ждем пока не будет сделан окончательный выбор
;; (let loop ((* #f))
;;    (unless (eq? (car selection) 4)
;;       (loop (sleep 8))))
(print "ok.")

; тут надо пересоздать окна и обновить обработчик клавиатуры
; TBD.
(print ".")

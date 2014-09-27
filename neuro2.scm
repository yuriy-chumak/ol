; http://habrahabr.ru/post/144881/

; парочка служебных макросов - помощников
(define (nth list n) ; начиная с 1
   (if (= n 1) (car list)
               (nth (cdr list) (- n 1))))


; итак, попытаемся научить нейрон распознавать буквы алфавита
; 1. Нам нужен массив нейронов по количеству букв:
;	a, b, c, ..., z - 
;abcdefghijklmnopqrstuvwxyz
;12345678901234567890123456
;         1         2
; 26 штук

; A-элементы, это сами нейроны
; S-элементы, это сенсонры - в нашем случае это штуки, которые отсылают 1, если в картинке есть черный цвет
;   иначе огтсылают 0 - ну то есть реагируют на сигнал в картинке
; Далее - синапы, это связи между S-элементом, и A-элементом.

; для начала попробуем поработать с картинкой 3*4
; x x x     1  2  3
; x x x     4  5  6
; x x x ==  7  8  9
; x x x    10 11 12
; x x x    13 14 15

; работаем с такими изображениями:
; 1:     2:     3:
; x x x  x x x  - - -
; x - x  x x x  x - x
; x - x  x x x  - x -
; x - x  x x x  x - x
; x x x  x x x  - - -

(define THRESHOLD 30) ; порог обучения - чем выше, тем точнее ответ, но тем и дольше обучать
(define (ANSWER axon-weights signal)
   (> (fold + 0 (zip * signal axon-weights)) THRESHOLD))
(define (PANSWER axon-weights signal)
   (/ (fold + 0 (zip * signal axon-weights)) THRESHOLD))


(define zero-input-vector '(0 0 0  0 0 0  0 0 0  0 0 0  0 0 0))

(define one '(1 1 1  1 0 1  1 0 1  1 0 1  1 1 1))
(define two '(1 1 1  1 1 1  1 1 1  1 1 1  1 1 1))

; а значит, нам надо 3 нейрона - A элемента
(define A '(60 60 60)) ; тут должны лежать пороговые веса возбуждения нейрона
                    ; чем он выше, тем больше будет точность работы нейрона, но тем дольше нам придется его обучать. 

; далее, нам надо 15 сенсоров - S элементов
;(define S-values '(0 0 0  0 0 0  0 0 0  0 0 0  0 0 0))
; сюда надо класть one или two

; аксон, это связь между сенсором и нейроном, каждый аксон содержит весовой коээфициент.
;  таким образом, аксон передает в нейрон свой вес, если на "его" сенсоре появилась "1".

(define one-values '(0 0 0  0 0 0  0 0 0  0 0 0  0 0 0)) ; веса аксонов первого нейрона
(define two-values '(0 0 0  0 0 0  0 0 0  0 0 0  0 0 0)) ; веса аксонов второго нейрона

; эта функция принимает на вход сигнал с сенсоров и веса аксонов нейрона, а на выход дает результат возбуждения нейрона
(define (S s values)
   (fold + 0 (zip * s values)))

;;; А теперь процесс обучения
; если нейрон ошибся, то корректируем веса так:
;  если нейрон не угадал, то прибавляем значения входов к весам соответствующих синапсов
;  если наоборот, угадал ложно - то вычитаем 
; кстати, этот момент спорный. тут, наверное, надо было бы добавлять случайные значения.

;корректировка:
(define one-v (zip + one-values one))

; так сохраняются веса нейронов: '(vector->file (list->vector (fasl-encode one-v))) "one.fasl")'


; попробуем занять обучением первого нейрона
; на вход ему будем подавать 10 разных паттернов

; в cdr находится список правильных ответов для каждого нейрона
; обучающие последовательности для них одинаковые
(define patterns '(
   ((0 0 0  0 0 0  0 0 0  0 1 0  0 0 0) . (0 0))
   ((1 1 1  1 0 1  1 0 1  1 0 1  1 1 1) . (1 0))
   ((1 1 1  0 0 0  0 0 0  0 0 0  0 0 0) . (0 0))
   ((0 0 0  1 1 1  0 0 0  0 0 0  0 0 0) . (0 0))
   ((1 1 1  0 0 0  1 1 1  0 0 0  1 1 1) . (0 0))
   ((0 0 0  0 0 0  1 1 1  0 0 0  0 0 0) . (0 0))
   ((0 0 0  0 0 0  0 0 0  1 1 1  0 0 0) . (0 0))
   ((0 0 0  0 0 0  1 1 1  1 1 1  0 0 0) . (0 0))
   ((0 0 0  0 0 0  0 0 0  0 0 0  1 1 1) . (0 0))
   ((1 1 1  1 1 1  1 1 1  1 1 1  1 1 1) . (0 1))
   ((0 0 0  1 1 1  1 1 1  0 0 0  0 0 0) . (0 0))
   ((1 0 1  1 0 1  1 0 1  1 1 1  0 0 0) . (0 0))
   ((1 1 1  0 0 0  1 1 1  1 1 1  0 0 0) . (0 0))
   ((0 0 0  0 0 0  0 0 0  0 0 0  1 1 1) . (0 0))
))

; обучающие паттерны готовы
; итак, прогоним цикл три раза :)

; matrix - список весов аксонов
; pattern - список паттернов c результатом (список пар)
; answer - список правильных ответов
(define (learn pattern answer matrix)
   (if (null? pattern) ; паттерны закончились, вернем подкорретированную матрицу
      matrix
     (let ((sensor (car pattern))
           (matrix-result (ANSWER (car pattern) matrix))
           (is-pattern-good (car answer)))
;        (print "testing pattern " (car pattern))
        (if (and matrix-result
             (= is-pattern-good 0))
          ; сказал "да" на плохой паттерн - повторим обучение до победы
          (begin
;            (print "- bad pattern good answer, matrix: " matrix) 
            (learn pattern answer (zip - matrix sensor)))
          (if (and (not matrix-result)
               (= is-pattern-good 1))
            ; сказал "нет" на хороший паттерн - повторим обучение
            (begin
;              (print "- good pattern bad answer, matrix: " matrix) 
              (learn pattern answer (zip + matrix sensor)))
            ; иначе все ок - можно переходить к следующему паттерну c той же матрицей
            (begin
;              (if (= is-pattern-good 1)
;                (print "+ good pattern good answer, matrix: " matrix)
;                (print "+ bad pattern bad answer, matrix: " matrix))
              (learn (cdr pattern) (cdr answer) matrix)))))))

;(define one-v (learn (map car patterns) (map cdr patterns) zero-input-vector))

; пройдемся по этому списку, скажем, 20 раз - 20 процессов обучения
;(define (times counter matrix patterns answers)
;  (define (time counter matrix)
;   (if (= counter 0)
;      matrix
;      (time (- counter 1) (learn patterns answers matrix))))
;  (time counter matrix))

(define (times counter matrix patterns answers)
  (let time ((counter counter) (matrix matrix))
   (if (= counter 0)
      matrix
      (time (- counter 1) (learn patterns answers matrix)))))



(define one-v (times 200 zero-input-vector (map car patterns) (map (lambda (list) (nth list 1)) (map cdr patterns))))
(define two-v (times 200 zero-input-vector (map car patterns) (map (lambda (list) (nth list 2)) (map cdr patterns))))

(print "result matrix is: " one-v)
(print "result matrix is: " two-v)

(print "а теперь тесты: ")
(print "one-v on one: " (PANSWER one-v one))
(print "one-v on two: " (PANSWER two-v one))
(print)
(print "two-v on one: " (PANSWER one-v two))
(print "two-v on two: " (PANSWER two-v two))

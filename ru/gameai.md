---
layout: page
title:  Game AI example
date: 2016-11-28 15:51:54 UTC
categories: ru
---

   В этом разделе мы попробуем продемонстрировать возможность использования Otus Lisp в задачах создания игрового искусственного интеллекта на примере нахождения пути в лабиринте.

   Давайте попробуем создать небольшой ИИ, который сможет находить дорогу в лабиринте на основании неполного знания карты. Такое себе "похожее" на нормальное поведение человека.

   В качестве визуализатора используем описанную в другом разделе библиотеку [OpenGL](?ru/opengl). Исходный код примера можно взять на [официальном сайте](https://github.com/yuriy-chumak/ol/tree/master/tutorial/X.%20Path%20Finder) проекта.

   Для начала, нам нужна пара простых функций рисования и скелет, который выводит карту в окно.

   Зададим лабиринт двумерным списком (матрицей) и сразу выставим размерные параметры:
<pre><code data-language="ol">
; схема лабиринта
(define scheme '(
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (1 0 1 0 0 0 0 0 0 0 0 1 0 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 1 1 1 0 0 1)
   (1 0 1 1 1 1 1 1 1 0 1 0 1 0 0 1)
   (1 0 1 1 1 1 1 1 1 0 1 0 1 0 0 1)
   (1 0 1 0 0 0 0 0 1 0 1 0 1 0 0 1)
   (1 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1)
   (1 0 1 1 1 1 1 1 1 1 1 0 1 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 1 0 1 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1)
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(define WIDTH (length (car scheme)))
(define HEIGHT (length scheme))
</code></pre>

   И добавим парочку утилитарных функций - получения N-того элемента, получение значения из матрицы и вывод "квадратика" на экран.

; и пара утилитарных функций
(define (nth list n)
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))

(define (at x y)
   (nth (nth scheme y) x))
(define (at2 x y scheme)
   (nth (nth scheme y) x))

(define (quad x y)
   (glVertex2f x y)
   (glVertex2f x (+ y 1))
   (glVertex2f (+ x 1) (+ y 1))
   (glVertex2f (+ x 1) y))
</code></pre>

#### ИИ как отдельный инстанс

   Было бы хорошо иметь возможность запускать на карту столько ИИ, сколько нам захочется, и чтобы они были вполне независимыми друг от друга - короче, нам нужен полноценный объектный подход. Мы довольно легко можем организовать его с помощью [сопрограмм](?ru/subprograms).

   Создадим функцию new-creature, которая запускает новую сопрограмму и объявляет несколько сообщений, с помощью которых мы сможем управлять данным конкретным ИИ.

<pre><code data-language="ol">
(define (new-creature x y)
(let* ((id (generate-unique-id))
       (creature (fork-server id (lambda ()
; x, y - положение AI
(let this ((fov (repeat (repeat 0 WIDTH) HEIGHT))
           (x x) (y y))
(let* ((envelope (wait-mail))
       (sender msg envelope))
   (tuple-case msg
      ((die)
         #false)
      ((set-location location)
         (this fov (car location) (cdr location)))
      ((get-fov)
         (mail sender fov)
         (this fov x y))
      ((get-location)
         (mail sender (cons x y))
         (this fov x y))
      ; обновить "свою" карту мира (fov) на основе заданной карты (map)
      ((update-fov map)
         (let ((fov
            ;...
         (this fov x y)))
      ; двигаться
      ((move dx dy)
         (this fov (+ x dx) (+ y dy)))
      ; построить путь к клетке (to-x to-y)
      ((A* to-x to-y)
         (mail sender
            (tuple 0 0 #empty #empty))
         (this fov x y))
      (else
         (print "'ai error: unknown command " msg)
         (this fov x y)))))))))
   (print "new creature " id " spawned.")
   id))
</code></pre>

   Здесь у нас несколько сигналов:
  * (die) - убить ИИ
  * (set-location '(x . y))- переместить ИИ в заданные координаты карты
  * (get-location) - узнать координаты, по которым находится ИИ
  * (get-fov) - попросить карту, которую "помнит" ИИ и на основании которой он будет решать куда ему двигаться, если попросят
  * (update-fov map) - попросить ИИ осмотреться (в параметрах передается актуальная карта)
  * (move dx dy) - двигаться, в параметрах дельта
  * (A* x y) - приказать ИИ найти путь к координатам и вернуть дельту, на которую ИИ считает ему стоит пойти с текущего места; дополнительно с целью визуализации процесса "мышления" возвращаются списки поиска (об этом позже - в месте, где будет рассказано об алгоритме A-star)

##### Алгоритм Брезенхейма

   В качестве первого "большого" алгоритма реализуем процесс обновления знаний ИИ о текущей карте. Для этого ИИ будет осматриваться вокруг себя и отмечать через какие из клеток он может ходить. Заметим, что при проверке клеток мы будем учитывать их прямую видимость из места, где находится ИИ - другими словами ИИ не будет иметь проникающее сквозь стены зрение.

   Для этого вполне неплохо подойдет алгоритм Брезенхейма для рисования линий. Мы просто попытаемся "нарисовать" линию из точки, где находится ИИ в угловую точку клетки. Если угловую клетку "видно", значит ИИ теперь знает, что находится в четырех смежных точках - стены или пустой пол.


   Теперь создадим окно и нарисуем в нем все, что нам надо: карту, которую "видел" созданный нами персонаж, его текущее положение и каждую секунду будем его сдвигать к нужной нам точке "выхода".

   Эта функция будет побольше, чуть позже я ее объясню:
<pre><code data-language="ol">
(define Context (gl:Create "Pathfinder sample"))

(gl:run
   Context

; init
(lambda ()

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity) ; вот тут мы зеркально отражаем карту сверху вниз
   (glOrtho -1 (+ WIDTH 1) (+ HEIGHT 1) -1  -1 1)
   (glMatrixMode GL_MODELVIEW)

   (list #empty))

; draw
(lambda (userdata)
(let (;(x (get userdata 'x 1.5))
      ;(y (get userdata 'y 1.5))
      (old-time (get userdata 'old-time 0)))

   (glClearColor 0.2 0.2 0.2 1.)
   (glClear GL_COLOR_BUFFER_BIT)

   (let*((new-time _ (clock))) (if (> new-time old-time) (begin

      ; передвинемся в случайном направлении:
      (let ((xy (interact me (tuple 'A* 14 1))))
         (mail me (tuple 'move (car xy) (cdr xy))))

      (mail me (tuple 'update-fov scheme))
   ))

   ; нарисуем карту, которую "помнит" создание
   (let ((map (interact me (tuple 'get-fov))))
      (glColor3f 0 1 0)
      (glBegin GL_QUADS)
      (for-each (lambda (i)
         (for-each (lambda (j)
            (if (> (at2 i j map) 0)
               (let ((age (at2 i j map))) ; как давно оно клетку "видело"
                  (let ((color (/ 1.0 (/ age 7))))
                  (glColor3f color color color)
                  (quad i j)))
               (begin
                  (glColor3f 0 0 0)
                  (quad i j))
            ))
            (lrage 0 1 HEIGHT)))
         (lrage 0 1 WIDTH))
      (glEnd))

      ; нарисуем, где сейчас наше создание находится:
      (glPointSize 6)
      (glBegin GL_POINTS)
      (let ((xy (interact me (tuple 'get-location))))
         (glPointSize 9.0)
            (glColor3f 1 0 0)
            (glVertex2f (+ (car xy) 0.5) (+ (cdr xy) 0.5))
            (glColor3f 0 1 0)
            (glVertex2f 14.5 1.5))
      (glEnd)
      (glPointSize 0)

      ; вернем модифицированные параметры
      (list (put userdata 'old-time new-time)))))
</code></pre>

#### А* алгоритм

   Алгоритмов поиска пути существует достаточное количество, с разной степерью применимости в каждых конкретных условиях. Так как у нас пример, ориентированный на простоту и понимаемость, то воспользуемся самым популярным алгоритмом - A* (A-star). В сети можно найти много руководст и статей по его реализации.

А теперь подпрограмма обработки команд "создания" (creature):
 * (set-location localtion) - передвинуть сознание в заданные координаты
 * (get-fov) - получить карту, как ее видит создание. 0 - пустая клетка, 1..x - как давно тут было видно стену.
 * (update-fov map) - обновить карту создания
 * (move dx dy) - передвинуться на dx,dy клеток в сторону (если это возможно - не мешают стены)
 * (A* to-x to-y) - самый интересный алгоритм, возвращает '(dx.dy), куда надо сдвинуться созданию, чтобы попытаться приблизиться к точке '(to-x.to-y), используя алгоритм поиска пути A* по неполной карте (той, которую видело и помнит создание).

Вот этот A* алгоритм, как самый интересный, я и приведу в следующем блоке:
<pre><code data-language="ol">
(let ((xy (cons x y))

      (hash (lambda (xy)
         (+ (<< (car xy) 16) (cdr xy))))
      (floor? (lambda (x y)
         (eq? (at2 x y fov) 0))))
(print "********************************************************************")

(mail sender
(let step1 ((n 21);(xy (cons x y))
            (c-list-set #empty)
            (o-list-set (put #empty (hash xy)  (tuple xy #f  0 0 0))))
   ; else поищем куда-бы перейти
   ; найдем клетку с минимальной стоимостью F
   (let*((f (ff-fold (lambda (s key value)
                        ;(print "ff-fold: " s " - " value)
                        (if (< (ref value 5) (car s))
                           (cons (ref value 5) value)
                           s))
               (cons 9999 #f) o-list-set))
         (_ (print "next: " f))
         (xy (ref (cdr f) 1)) ; положение клетки с минимальным весом '(x.y)
         ; перенесем ее из открытого в закрытый список
         (o-list-set (del o-list-set (hash xy)))
         (c-list-set (put c-list-set (hash xy) (cdr f))))

      (if (and
            (eq? (car xy) to-x)
            (eq? (cdr xy) to-y))
         ;(eq? n 0)
         (let rev ((xy xy))
            (print "xy: " xy)
            (let*((parent (ref (get c-list-set (hash xy) #f) 2)) ; переделать
                  (parent-of-parent (ref (get c-list-set (hash parent) #f) 2)))
               (if parent-of-parent (rev parent)
                  (cons
                     (- (car xy) (car parent))
                     (- (cdr xy) (cdr parent))))))
         ;(cons o-list-set c-list-set) ;  todo: вернуть только первый шаг (развернув путь)

         ; 5: Проверяем все соседние клетки.
         (let*((x (car xy))
               (y (cdr xy))

               (_ (print x " :: " y " ^ " (hash xy)))
               (_ (print "c-list-set: " c-list-set))
               (_ (print "o-list-set: " o-list-set))

               (o-list-set (fold (lambda (n v)
                              (if (and
                                    (floor? (car v) (cdr v)) ; если туда можно передвинуться...
                                    (eq? #f (get c-list-set (hash v) #f)))
                                 (let ((G (+ (ref (get c-list-set (hash xy) #f) 3) 1)); G родителя + 1
                                       ;H calculated by "Manhattan method"
                                       (H (* (+ (abs (- (car v) to-x))
                                             (abs (- (cdr v) to-y))) 2))
                                       ; 6: Если соседняя клетка уже находится в открытом списке
                                       (got (get o-list-set (hash v) #f)))

                                    (print "ready to open: " v "(" xy ") - H:" H ", got:" got)

                                    ; если эта клетка уже в списке
                                    (if got
                                       (if (< G (ref got 3)) ; но наш путь короче
                                          (put n (hash v)  (tuple v xy  G H (+ G H)))
                                          ;else ничего не делаем
                                          n)
                                       ; else
                                       (put n (hash v)  (tuple v xy  G H (+ G H)))))
                                 n))
                              o-list-set (list
                                             (cons x (- y 1))
                                             (cons x (+ y 1))
                                             (cons (- x 1) y)
                                             (cons (+ x 1) y))))


         )
            (step1 (- n 1) c-list-set o-list-set)))))))
</code></pre>

#### Карта

   Наш ИИ будет несколько "умнее" своих собратьев тем, что будет вести себя "глупее" них. Всех, наверное, раздражали игровые враги, которые всегда знают куда надо двигаться, словно им сразу выдали точную карту помещений и оперативно сообщают о всех изменениях. Так вот, наш ИИ будет самостоятельно осматриваться и искать лучший, со своей точки зрения путь. Все, что он еще не видел - в расчете пути будет игнорироваться. Кроме того, со временем "знания" о карте ИИ будут устаревать и тоже игнорироваться.



   Для начала нам нужна реализация алгоритма Брезенхейма в наших условиях:
<pre><code data-language="ol">
(define (horizontal-bresenham x1 y1  x2 y2  dk)
(let* ((x (+ 1 (floor x1)))
       (y (+ y1 (* (- x x1) dk))))
(let loop ((x x) (y y) (oldy -1)  (n (- (ceil x2) x)))
   (if (= n 0)
      #t
   (if (and.
          (not (= y oldy)) ; если пересекли линию стен
          (> (at (- x 1) (floor y)) 0))
      #f
   (if (> (at x (floor y)) 0)
      #f
   (loop (+ x 1) (+ y dk) y (- n 1))))))))

(define (vertical-bresenham x1 y1  x2 y2  dk)
(let* ((y (+ 1 (floor y1)))
       (x (+ x1 (* (- y y1) dk))))
(let loop ((x x) (y y) (oldx -1)  (n (- (ceil y2) y)))
   (if (= n 0)
      #t
   (if (and.
          (not (= x oldx)) ; если пересекли линию стен
          (> (at (floor x) (- y 1)) 0))
      #f
   (if (> (at (floor x) y) 0)
      #f
   (loop (+ x dk) (+ y 1) y (- n 1))))))))

(define (is-point-can-see-point x1 y1  x2 y2)
; подразумевается, что начальная и конечная точки НЕ в стене
;(if (and (= (floor x1) (floor x2)) (= (floor y1) (floor y2))) ; если это один и тот же блок
;   #t
(let ((dx (- x2 x1))
      (dy (- y2 y1)))
   (if (> (abs dx) (abs dy)) ; горизонтальный
      (if (> dx 0)
         (horizontal-bresenham x1 y1  x2 y2  (/ dy dx))
         (horizontal-bresenham x2 y2  x1 y1  (/ dy dx)))
      (if (> dy 0)
         (vertical-bresenham x1 y1  x2 y2  (/ dx dy))
         (vertical-bresenham x2 y2  x1 y1  (/ dx dy))))))

(define (is-visible x1 y1 x2 y2)
   (and (> x1 0) (> y1 0) (< x1 WIDTH) (< y1 HEIGHT)
        (> x2 0) (> y2 0) (< x2 WIDTH) (< y2 HEIGHT)
        (is-point-can-see-point x1 y1 x2 y2)))
</code></pre>

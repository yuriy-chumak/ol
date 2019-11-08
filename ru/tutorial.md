---
layout: page
title:  Tutorial
date: 2016-03-17 12:50:03 UTC
categories: ru
---
> Внимание, эта статья находится в процессе создания; ее содержание может (и будет) меняться, пока полностью не удовлетворит автора. А до тех пор я не ручаюсь за стопроцентную достоверность приведенной информации.


Tutorial разбит на логические части, частично повторяющие структуру [R<sup>7</sup>RS](http://r7rs.org){:target="_blank"} Scheme.

В дальнейшем тексте будут встречаться отсылки к Lisp, когда это будут общие для всего семейства диалектов вещи; Scheme, когда надо будет обратить внимание на различия в реализации Scheme и Common Lisp (или других диалектов); и Ol, когда это будет специфичное для представляемого диалекта поведение.

1. [Введение](?ru/overview)
   * Семантика, синтаксис, терминология
1. [Лексические соглашения](?ru/lexical-conventions)
   * Идентификаторы, комментарии, и т.д.
1. [Глоссарий](?ru/glossary)
1. [Базовые концепции](?ru/basic-concepts)
   * Атомы и объекты, переменные, ключевые слова и области видимости, типы
1. Стандартные процедуры
   * Предикаты эквивалентности
   * Числа
   * Булевы константы и булева алгебра
   * Списки и пары
   * Символы (идентификаторы)
   * Строки
   * Векторы
   * Байтвекторы
   * Управляющие конструкции
   * Исключения(?)
   * Вычисчлители
   * Ввод и вывод
   * Системный (нейтивный) интерфейс
1. Стандартные библиотеки
1. Высокоуровневые конструкции языка
   * [Сопрограммы](?ru/coroutines)
1. [Внутреннее устройство виртуальной машины](?ru/internals)
1. [Примеры](?ru/examples)
   1. [OpenGL](?ru/opengl)
   1. [Matrix](?ru/matrix)
   1. [HTTP Server](?ru/http-server)
   1. [Game AI](?ru/gameai)


## конспект занятий

* REPL: Read-Eval-Print Loop
   * Чтение-Вычисление-Печать Цикл (ЧВПЦ)
   * примеры ввода в repl чисел, простых математических операций (+, -, *), простых математических функций (expt)
   * объяснение использования скобочек

* Создание файлов с программой
   * mc -> Shift+F4, F4, F2
   * vscode -> open folder, open console, install syntax highlighter
   * #!/usr/bin/ol
   * chmod +x

* Списки
   * лисп - это списки и обработка списков
      * пример организации списков [в памяти](https://d2vlcm61l7u1fs.cloudfront.net/media%2Ffda%2Ffda36e53-c6d1-47c8-88b7-9a418d0f7e84%2FphpFNhftT.png)
   * три способа задания списков (', `, list)
      * '(1 2 3) - автоматически квотирует все аргументы
      * `(1 2 3) - то же, что и выше, но позволяет блокировать квотирование с помощью запятой (,)
      * (list 1 2 3) - функция создания списков
   * аналоги списков (вектор, кортеж)
      * вектор - в отличие от списков, это набор непрерывных блоков однотипных данных; позволяет быстро доступаться до любого элемента, не позволяет легко изменять размеры
         * (vector 1 2 3)
         * (vector 1 "we" 3)
      * байтвектор (bytevector) - вектор из байтов - маленьких чисел (0..255)
      * кортеж (tuple) - непрерывный блок разнородных данных, аналог вектора
         * (tuple 'me 1 2 "hello" 3+8i)
   * пары (частный случай списков) - cons
   * где смотреть примеры - [rosettacode](https://rosettacode.org/wiki/Category:Ol), [тесты](https://github.com/yuriy-chumak/ol/tree/master/tests).
   * получение головы списка, хвоста списка или некоторого элемента списка (car, cdr, cadr, caddr, ..., list-ref)

* Простые типы данных
   * числа ("атомарные", натуральные, вещественные, рациональные, комплексные, иррациональные) - детальнее [тут](?ru/internals/numbers).
   * строки (ansi, юникод)
   * буквы и знаки алфавита (characters), в ol именуются "рунами" (rune) и являются натуральными числами. Для перевода в текстовый формат используется функция string: (string #\;) => ";", (string 1992) => "ф"
   * символы (в смысле - элементы словаря лиспа), задаются либо словом quote (от него слово "квотирование"), либо апострофом. 'myname, (quote myname)
   * словарь (ff), полное наименование - ассоциативный массив (associative array)
   * деление на value- и reference- значения

* Связывание (именование) значенией; синонимы: присвоение значения, задание константы
   * (define x 123)

* Функции (сложная тема)
   * (define f (lambda (x) (+ x x)))
   * (define (f x) (+ x x))

* Условные и безусловные блоки выполнения
   * if, unless
   * case, cond
   * begin
   * tuple-case

* Рекурсия
   * (define (nth l n)
        (if (= n 1)
           (car l)
           (nth (cdr l) (- n 1))))

* Выбрать игру для реализации в качестве учебного примера, например
   * [Builder Dash](http://zxgames.com/image/screenshots/boulder-dash-episode-1/index_1024x768.png)
   * [Lode Runner](http://www.zxgames.com/image/screenshots/lode-runner-episode-1/3_600x450.png)
   * [Tetris](http://1.bp.blogspot.com/-9dmm36JrIII/T-Qcd3eve2I/AAAAAAAABAk/xIQbiCxJCOM/s1600/tetris.jpg)
   * [2048](http://3.bp.blogspot.com/-ApnlPewWE9o/UzsQYKiTHEI/AAAAAAAAeeA/xlLT_EMsGLw/s1600/pre-2048.jpg)
   * [Minesweeper](http://i.kinja-img.com/gawker-media/image/upload/s--N7w5VHcw--/18j2akmczwjmnjpg.jpg)
   * [Angband](http://img03.deviantart.net/f463/i/2013/105/9/3/return_to_angband_by_stirzocular-d61qv2h.jpg) - Console or GUI version
   * [Chess](http://3.bp.blogspot.com/-Wd25JDAQpt8/UcQvbD5BF_I/AAAAAAAAa4Q/hkrDS7dtWqg/s1600/Chess+HD+Pictures8.jpg) - Console or GUI version
   * свой выбор


## все еще в процессе


   Some works more.

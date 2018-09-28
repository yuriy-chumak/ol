---
layout: page
title:  Tutorial
date: 2016-03-17 12:50:03 UTC
categories: ru
---
   Tutorial разбит на логические части, частично повторяющие структуру [R5RS](http://www.schemers.org/Documents/Standards/R5RS/){:target="_blank"} Scheme.
В контексте будут использоваться отсылки к Lisp, когда это будут общие для всего семейства диалектов вещи; Scheme, когда надо будет обратить внимание на различия в реализации Ol и Scheme; и Ol, когда это будет специфичное для представляемого диалекта поведение.

## план занятий

* Списки
   * лисп - это списки и обработка списков
      * пример организации списков [в памяти](https://d2vlcm61l7u1fs.cloudfront.net/media%2Ffda%2Ffda36e53-c6d1-47c8-88b7-9a418d0f7e84%2FphpFNhftT.png)
   * скобочки
   * три способа задания списков (', `, list)
      * '(1 2 3) - автоматически квотирует все аргументы
      * `(1 2 3) - то же, что и выше, но позволяет блокировать квотирование с помоью запятой (,)
      * (list 1 2 3) - функция создания списков
   * какие еще бывают списки (веткор, кортеж)
      * вектор - в отличие от списков, это [набор] непрерывных блоков данных
         * (vector 1 2 3)
         * (vector 1 "we" 3)
      * байтвектор (bytevector) - непрерывный блок байтов
      * кортеж (tuple) - непрерывный блок данных
         * (tuple 'me 1 2 "asas" 888)
   * пары (частный случай списков) - cons
   * где смотреть примеры - [rosettacode](https://rosettacode.org/wiki/Category:Ol), [тесты](https://github.com/yuriy-chumak/ol/tree/master/tests).
   * получение головы списка, хвоста списка или некоторого элемента списка (car, cdr, cadr, caddr, ...)

* Атомы, типы атомов
   * числа ("атомарные", натуральные, вещественные, рациональные, комплексные,  иррациональные) - детальнее [тут](?ru/internals/numbers).
   * символы (в смысле - буквы и знаки алфавита), в ol символы являются просто числами. Для перевода в текстовый формат используется функция string: (string #\;) => ";"
   * строки (ansi, юникод)
   * пары '(1 . 2)
   * символы (в смысле - элементы словаря лиспа)
   * словарь (ff)
   * value- и reference- значения

* Связывание (именование) значенией
   * (define x 123)

* Функции (сложная тема)
   * (define (f x) (+ x x))
   * (define f (lambda (x) (+ x x)))

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

* создание файлов с программой
   * mc -> F4, F2
   * #!/usr/bin/ol
   * chmod +x

## предварительный дизайн версии angband

* актор
   * сопрограммы
      * (fork-server ...)
      * (wait-mail ...)
   * рекурсия
   * кортежи как команды
      * (tuple ...)
      * (tuple-case ...)

В качестве примера заведем одного актора - волшебника (wizard), который будет уметь:
* передвигаться в клетку '(x . y)
   * в будущем добавим алгоритм A* поиска пути для перехода в нужную клетку
* сложить в инвентарь предмет (сообщить о результате - смогли ли сложить)
* получить список предметов в инвентаре
* использовать предмет из инвентаря (сообщить о результате)
* узнать состояние актора
* ...


* рисуем поле
* сопрограмма как объектная модель актора
* ...


## все еще в процессе

   Каждый из примеров можно сразу проверить, введя его в представленный на странице терминал.

1. [Введение](?ru/overview)
   1. [Лексические соглашения](?ru/lexical-conventions)
   1. [Общее представление](?ru/basic-concepts)
      1. [Переменные, ключевые слова и область видимости](?ru/basic-concepts/Variables-keywords-regions)
1. [Внутреннее устройство виртуальной машины](?ru/internals)
1. [Примеры](?ru/examples)
   1. [OpenGL](?ru/opengl)
   1. [Matrix](?ru/matrix)
   1. [HTTP Server](?ru/http-server)
   1. [Game AI](?ru/gameai)

   Some works more.

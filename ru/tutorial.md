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
   * скобочки
   * три способа задания списков (', `, list)
   * какие еще бывают списки (ветор, кортеж)
   * пары (частный случай списков) - cons
   * где смотреть примеры - [rosettacode](https://rosettacode.org/wiki/Category:Ol), [тесты](https://github.com/yuriy-chumak/ol/tree/master/tests).
   * получение головы списка, хвоста списка или некоторого элемента списка (car, cdr, cadr, caddr, ...)

* Атомы, типы атомов
   * числа ("атомарные", натуральные, вещественные, рациональные, комплексные,  иррациональные)
   * символы (в ol символы являются просто числами)
   * строки (ansi, юникод)
   * пары
   * символы
   * словарь (ff)
   * value- и reference- значения

* Связывание (именование) значенией
* Функции
* Рекурсия
* Выбрать игру для реализации в качестве учебного примера, например
   * [Builder Dash](http://zxgames.com/image/screenshots/boulder-dash-episode-1/index_1024x768.png)
   * [Lode Runner](http://www.zxgames.com/image/screenshots/lode-runner-episode-1/3_600x450.png)
   * [Tetris](http://1.bp.blogspot.com/-9dmm36JrIII/T-Qcd3eve2I/AAAAAAAABAk/xIQbiCxJCOM/s1600/tetris.jpg)
   * [2048](http://3.bp.blogspot.com/-ApnlPewWE9o/UzsQYKiTHEI/AAAAAAAAeeA/xlLT_EMsGLw/s1600/pre-2048.jpg)
   * [Minesweeper](http://i.kinja-img.com/gawker-media/image/upload/s--N7w5VHcw--/18j2akmczwjmnjpg.jpg)
   * [Angband](http://img03.deviantart.net/f463/i/2013/105/9/3/return_to_angband_by_stirzocular-d61qv2h.jpg) - Console or GUI version
   * [Chess](http://3.bp.blogspot.com/-Wd25JDAQpt8/UcQvbD5BF_I/AAAAAAAAa4Q/hkrDS7dtWqg/s1600/Chess+HD+Pictures8.jpg) - Console or GUI version
   * свой выбор
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

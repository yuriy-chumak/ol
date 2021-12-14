---
layout: page
title:  Делаем простой рогалик
categories: tutorial
---
> Рогалик (Roguelike) -  жанр компьютерных игр. Характерными особенностями классического roguelike являются пошаговость и необратимость смерти персонажа — в случае его гибели игрок не может загрузить игру и должен начать её заново.
> <br/> <span style="float: right;">[Материал из Википедии](https://ru.wikipedia.org/wiki/Roguelike)</span>
<br/>

## Среда разработки

До начала работы нам надо подготовить среду разработки, куда будет входить систему управления версиями кода и интерпретатор используемого скриптового языка - Лиспа.
Подготовка зависит от используемой операционной системы. Сама же дальнейшая разработка от используемой ОС зависеть не будет, как и результат - который можно будет свободно запускать под любой поддерживаемой платформой (linux, windows, BSD, android, etc.).

Для Windows надо делать следующее:

* Создаем в любом удобном для нас месте папку "Game", пусть для конкретики это будет `C:\Game`. В дальнейшем все упоминания папки `C:\Game` надо понимать как именно эту вашу папку и вместо нее использовать именно этот ваш путь (естественно, если вы создали папку `C:\Game` - то ничего менять не надо, что я и рекомендую). Всю работу мы будем делать в этой папке, и только в этой папке.

* Скачиваем Git (он нам будет нужен для сохранения результатов работы и обновления используемого инструментария) по [ссылке](https://git-scm.com/downloads).

* Заходим в скачанный архив и запускаем инсталлятор. Ничего в инсталляторе не меняем, все устанавливаем как предложено по умолчанию. Ждем пока инсталлятор закончит работу.

* Устанавливаем обновляемые библиотеки нашего Лиспа, для чего:

  * Запускаем `cmd.exe` и переходим в нашу папку командами `C:` и `cd C:\Game`.

    Это окно либо не закрываем (оно нам еще понадобится), либо просто каждый раз наново открываем, не забывая две указанные команды для перемещения в нашу рабочую папку.

  * Проверяем, что мы действительно в нужной папке - в окошке `C:\WINDOWS\system32\cmd.exe` должно быть что-то вроде:

```
Microsoft Windows [Version 10.0.14393]
(c) 2016 Microsoft Corporation. All rights reserved.

C:\Users\me>C:

C:\Users\mr>cd C:\Game

C:\Game>
```
  * Выполняем сложную команду `git clone -b develop --depth 1 --single-branch https://github.com/yuriy-chumak/ol.git ol`.

  * Результат должен выглядеть как-то так:

```
C:\Game>git clone -b develop --depth 1 --single-branch https://github.com/yuriy-chumak/ol.git ol
Cloning into 'ol'...
remote: Counting objects: 435, done.
remote: Compressing objects: 100% (367/367), done.
remote: Total 435 (delta 20), reused 218 (delta 2), pack-reused 0
Receiving objects: 100% (435/435), 4.93 MiB | 764.00 KiB/s, done.
Resolving deltas: 100% (20/20), done.

C:\Game>
```

* Скачиваем предсобранный Лисп (в нашем случае это будет Otus Lisp) по [ссылке](https://github.com/yuriy-chumak/ol/releases). Нас интересует архив `ol.windows.x86_64.zip` в разделе `Downloads`.

* Распаковываем этот архив в папку `C:\Game\ol` (внимание, подпапка `ol` нашей папки для работы). В архиве должен быть только один файл `ol.exe`, его и распаковываем.

* Создаем в папке `C:\Game` пустой файл `naruto.scm`. Все, на этом все приготовления закончены, можно начинать.

## Первый шаг

Первым шагом нашей игры мы нарисуем цветной прямоугольник, после чего рассмотрим и обсудим все написанное. Следующими шагами будем наш код переделывать и усложнять.

Итак, открываем naruto.scm и записываем в него вот такую программу:

```lisp
#!/usr/bin/ol

(define *include-dirs* (cons "ol" *include-dirs*))
(import (lib rlutil))

(cls)
(define map (tuple
"########################################"
"#......................................#"
"#.#####....................#########...#"
"#.#......................###.......#...#"
"#.#...#......................#####.#...#"
"#.#####..................#......#..#####"
"#.........####.##........#####..#......#"
"#.........#.....#............#######.#.#"
"#.........#######................#...#.#"
"########################################"
))

(set-color BROWN)
(for-each (lambda (l) (print (ref map l))) (iota (size map) 1))

(locate 14 5)
(set-color LIGHTGREEN)
(print "@")

(locate 5 2)
(set-color RED)
(print "@")

(locate 36 9)
(set-color YELLOW)
(print "$")

(locate 1 12)
(set-color WHITE)
(display ">")
(read)
```

* Для запуска нам надо просто перейти в нашу рабочую папку (заранее или только что открытое окно `cmd.exe`) и выполнить `ol\ol naruto.scm`.

  Наблюдаем результат (цветной):

```
########################################
#...@..................................#
#.#####....................#########...#
#.#......................###.......#...#
#.#...#......@...............#####.#...#
#.#####..................#......#..#####
#.........####.##........#####..#......#
#.........#.....#............#######.#.#
#.........#######................#.$.#.#
########################################

>
```

  Для выхода из запущенной программы набираем любой текст (или цифры) на клавиатуре и жмем \[enter\]. Все, наш первый игровой скрипт готов. Дальше можно переходить к описанию того, что мы в нем написали и как это работает.
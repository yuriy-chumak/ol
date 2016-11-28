---
layout: page
title:  Digital Rain example
date: 2016-11-28 15:51:55 UTC
categories: ru
---

   В качестве теста мутаторов в основной репозиторий был добавлен пример [Matrix](https://github.com/yuriy-chumak/ol/tree/master/tutorial/Matrix), который рисует в окошке "цифровой дождь". Вывод настраивается с помощью нескольких констант:
   
  * SLIDING-MODE - стиль дождя
    * 0: без слайдеров
    * 2: только слайдеры
    * 1: смешанный
  * WIDTH - количество глифов по горизонтали
  * HEIGHT - количество глифов по вертикали
  * PHOSPHOR-ENABLED - включить свечение глифов
  * RANDGLOW-ENABLED - включить свечение слайдеров

  ![Скрин окна вывода](assets/matrix-64x56.png)

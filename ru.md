---
layout: page
title:  О проекте
categories: index
---
> Любая достаточно сложная программа содержит заново написанную, неспецифицированную, глючную и медленную реализацию половины какого-либо из диалектов Lisp.
> <br/> <span style="float: right;">Десятое правило Гринспена</span>

**Теперь мы и в Твиттере! Все актуальные новости о проекте на [@otus_lisp](https://twitter.com/otus_lisp).**


**Otus Lisp** (произносится как [́отэс лисп]), или сокращенно Ol ([́ол]) - чисто[\*](#pure) функциональный диалект языка Lisp.

Ol реализован как расширенное подмножество Scheme [R<sup>7</sup>RS](http://www.schemers.org/Documents/Standards/R5RS/), включая, но не ограничиваясь, некоторые из [SRFI](https://r7rs.org). Маленький, встраиваемый и кроссплатформенный; может работать в собственной песочнице (в системах, где песочница поддерживается); предоставляет портабельный, высокоуровневый путь для вызова написанного на других языках кода. Вы можете использовать его под GNU/Linux, Windows, всевозможных Unix, различных BSD, Android, webOS, macOS, Minoca и многих других операционных системах на различных архитектурах (x86/x86_64, arm/aarch64, mips, ppc, ...) платформах (ODroid, LattePanda и т.д.).


### Описание языка и список функций

[Все это находится по ссылке](https://github.com/yuriy-chumak/ol/blob/master/doc/reference/README.md)


### Исходный код
Исходный код проекта для изучения и самостоятельной сборки, а также инструкции по этому вполне несложному процессу можно забрать из [официального репозитария](https://github.com/yuriy-chumak/ol)
(смотрите секцию BUILD главного README).


### Протестированные платформы
- [x] x86: 80486, pentium, pentium 2, pentium 3, athlon, core 2 quad, core i3, core i5, core i7.
- [x] x86_64: core 2 quad, core i3, core i5, core i7.
- [x] aarch64: cortex-a53, cortex-a57, cortex-a75/a55.
- [x] arm: armv5tejl, armv7l, arm920t, snapdragon 801.
  - nearly planned, but not yet tested: arm926t, arm1136, cortex-a7, cortex-a9, cortex-a15, cortex-m3, cortex-m4.
- [x] powerpc: Mac G4
- [x] mips32: algor (P-5064)
- [x] mips64
- fully supported, but not yet tested: m68k, microblaze, or1k, ppc64, sh4, spark, spark64, ztensa.

### Протестированные операционные системы/устройства
- [x] GNU/Linux: CentOS, Debian, Fedora, RHEL, SLE, ScientificLinux, Uninvention, openSUSE, Ubuntu.
- [x] Windows: Windows 95, Windows 98, Windows ME, Windows NT, Windows 2000, Windows XP, Windows Vista, Windows 7, Windows 8, Windows 10.
- [x] Unix: OpenBSD, FreeBSD, NetBSD.
- [x] Android: all versions up to Android 10 (должны поддерживаться все, включая последнюю).
- [x] webOS: 2.0.
- [x] macOS: Majove (должны поддерживаться все, включая последнюю).
- [x] Odroid: [C1+](https://www.hardkernel.com/main/products/prdt_info.php?g_code=G143703355573).
- [x] Minoca [OS](https://www.minocacorp.com/product/).
- [x] [LattePanda](https://www.lattepanda.com/) (Win10, Ubuntu 16.04)

### Протестированные веб-браузеры (asm.js версия Ol)
- [x] Iceweasel 38.6.0 / Debian 6.0
- [x] Epiphany 3.4.2 / Debian 6.0
- [x] Chrome (37, 48, etc.) / Debian 6.0
- [x] Luakit 1.8.0 / Ubuntu 9.10
- [x] Opera 58.0.3135.65 / Windows 2008 R2
- [x] SeaMonkey 2.33 / Debian 6.0
- [x] Firefox (28, 36, 44, etc. ) / Debian 6.0
- [x] Chrome (39, 44, 51, 71, etc.) / Windows 2008 R2
- [x] Iceape 2.7.12 / Debian 6.0


### О проекте
Otus Lisp досупен под двумя лицензиями:
[MIT License](https://github.com/yuriy-chumak/ol/blob/master/LICENSE) и
[GNU ](https://github.com/yuriy-chumak/ol/blob/master/COPYING)([L](https://github.com/yuriy-chumak/ol/blob/master/COPYING.LESSER))[GPLv3 License](https://github.com/yuriy-chumak/ol/blob/master/COPYING).

Вот неполный список того, что может вам предоставить Ol:

* маленькая и быстрая [виртуальная машина](?ru/internals/olvm) (всего 42 КБ[\*](#42kb) в бинарном виде)
* высокоэффективный, благодаря чистой функциональности языка, [сборщик мусора](?ru/internals/memory)
* [встраиваемость](?ru/embed),
* кросплатформенность (Linux, Windows, BSD, Android, webOS, Web, macOS, etc.)
* поддержка различных архитектур (i586, amd64, arm, mips, etc.)
* поддержка 32- и 64-битных платформ
* поддержка little- и big-endian платформ
* возможность компилировать ol-скрипты в бинарный формат; а также, естественно, выполнять их
* встраиваемость
* динамическое связывание с функциями из "родных" [системных библиотек](?ru/ffi) напрямую (без использования других языков программирования, таких как C), а также [обратные вызовы](?ru/pinned) из них же
* числа неограниченной точности (точнее, ограниченной только размером доступной оперативной памяти)
* полный спектр интересных и полезных особенности из мира функционального программирования:
  * продолжения
  * хвостовая рекурсия
  * функции, как объекты первого класса
  * [сопрограммы](?ru/subprograms)
  * и т.д.
* ограниченные [сеттеры (мутаторы)](?ru/mutators), как маленькое приятное дополнение из императивного мира
* но что главное, это настоящий, концентрированный, ничем не разбавленный [Lisp](https://ru.wikipedia.org/wiki/%D0%9B%D0%B8%D1%81%D0%BF)!

Вы можете сразу попробовать ol в деле в терминале на этой же страничке без скачивания и установки любых бинарных файлов. Примеры можно посмотреть в конце этой страницы.


### Новости разработки <a name="news"></a>
* Смотрите акутальные новости на официальном [Twitter стриме](https://twitter.com/otus_lisp).

#### Infix Notation

Теперь вы можете использовать традиционную математическую нотацию пока программируете в Ol. `(\\ )` - сокращение для макроса "infix-notation".

Попробуйте несколько примеров:
<pre><code id="infix1" data-language="ol">(import (math infix-notation))

(print (\\ 2 + 3 * 5 - 7))
</code><button class="doit" onclick="doit(infix1.textContent)">send to the terminal</button></pre>

<pre><code id="infix2" data-language="ol">(import (math infix-notation))

(print (\\ (2 + 3) * (5 - 7)))
</code><button class="doit" onclick="doit(infix2.textContent)">send to the terminal</button></pre>

<pre><code id="infix4" data-language="ol">(import (math infix-notation))

(print (\\ 2 + sqrt(16)))
</code><button class="doit" onclick="doit(infix4.textContent)">send to the terminal</button></pre>

<pre><code id="infix5" data-language="ol">(import (math infix-notation))

(print (\\ 7! - 2/3))
</code><button class="doit" onclick="doit(infix5.textContent)">send to the terminal</button></pre>


### Сборка проекта и другие мелочи

Исходный код проекта для самостоятельной сборки, а также инструкции по этому вполне несложному процессу можно забрать из [официального репозитария](https://github.com/yuriy-chumak/ol)
(смотрите секцию BUILD главного README).

Otus Lisp распространяется в виде двух исполняемых бинарных файлов - самостоятельной виртуальной машины (vm для мира Posix, vm.exe для Windows) и компилятора/интерпретатора Ol (ol и ol.exe соответственно), а так же набора библиотек.
В качестве основной библиотеки выступает repl - реализация языка Otus Lisp поверх базовых примитивов виртуальной машины.

Командные строки для vm и ol совпадают с единственным отличием - виртуальная машина принимает только бинарные (предварительно скомпилированные) скрипты, в то время как интерпретатор принимает и бинарные, и текстовые.

Примеры использования:

* "./vm repl" - запустить интерактивную сессию REPL ("repl" - скомпилированный язык Otus Lisp, лежит в бинарном виде в корневой папке проекта), выполнять ввод команд с консоли (клавиатуры)
* "./ol" - аналог "./vm repl"
* "./ol program.scm" - выполнить program.scm в песочнице
* "./ol <program.scm - аналог предыдущей команды, выполнить program.scm в песочнице
* "./ol --home=/home/me/ol" - установить путь к библиотекам Otus Lisp как /home/me/ol и запустить интерактивную сессию REPL


### Мои заметки
* [О бинарных пакетах в NetBSD](?blog/2016.03.17-13.06)
* [Тестируем на 80486](?blog/2017.07.26-15.51)


### [Обучалка](?ru/tutorial)

Например, попробуйте этот пример:
<pre><code id="fold1" data-language="ol">(fold * 1 (iota 100 1 1))</code><button class="doit" onclick="doit(fold1.textContent)">отправить в терминал</button></pre>

Нажав "отправить в терминал" или введя пример вручную вы должны получить красивое длинное число - результат вычисления факториала 99.

Или вот пример посерьезнее: как насчет числа Пи? Я имею ввиду вычислить первые 200 цифр числа Пи? А может хотите 1000? Легко (для вас - легко, а вашему компьютеру придется потрудиться; так что пожалуйста, имейте терпение - на более слабых компьютерах это может занять некоторое время).
<pre><code id="samplePi" data-language="ol">(define (pi total-digits)
   (let loop ((q 1) (r 0) (t 1) (k 1) (n 3) (l 3) (digits total-digits))
      (unless (eq? digits 0)
         (if (< (- (+ (* 4 q) r) t) (* n t))
            (begin
               (display n) (if (eq? digits total-digits) (display "."))
               (loop (* q  10)
                     (* 10 (- r (* n t)))
                     t
                     k
                     (- (div (* 10 (+ (* 3 q) r)) t) (* 10 n))
                     l
                     (- digits 1)))
            (begin
               (loop (* q k)
                     (* (+ (* 2 q) r) l)
                     (* t l)
                     (+ k 1)
                     (div (+ (* q (* 7 k)) 2 (* r l)) (* t l))
                     (+ l 2)
                     digits)))))
    (print))

(pi 200)</code><button class="doit" onclick="doit(samplePi.textContent)">отправить в терминал</button></pre>


По [ссылке](?ru/tutorial) можно почитать детальнее про проект, а так же как программировать под ol и детали внутренней реализации. В общем все, что касается непосредственно языка и виртуальной машины (документация в процессе наполнения).


### p.s.

* <a name="pure"></a>На самом деле Otus Lisp немного расширен в сторону традиционного императивного программирования, а именно введены лимитированные [мутаторы](?ru/mutators). Мутаторы используются в генераторе псевдослучайных чисел. Кроме того мутаторы могут помочь ускорить типично императивные вещи (например, изменение одного элемента большого списка без копирования его частей).
* <a name="42kb"></a>Для разных операционных систем размер виртуальной машины может различаться как в большую, так и в меньшую стороны (например, для win64 x86_64 - 79 КБ, для ubuntu64 armhf - 30 KB). 42kb - типичный размер для Linux без включения дебаг-информации, ffi и экспериментальной библиотеки поддержки inexact математики (но включая внутренние проверки целостности, сокеты, вызовы "родных" системных функций и песочницу).
* <a name="sandbox"></a>Режим песочницы поддерживается только для операционных систем, в которых реализован [SECCOMP](https://en.wikipedia.org/wiki/Seccomp) (secure computing mode), а это GNU/Linux (и разные его дистрибутивы, Ubuntu, Debian, Suse, etc.). Ведется работа по поддержке песочницы *BSD систем.

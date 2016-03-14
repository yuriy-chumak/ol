---
layout: page
title:  Числа
date: 2016-03-14 18:10:12 UTC
categories: en
---

   Числа, это те редкие элементы языка ol, которые могут передаваться как по значению (by value), так и по ссылке (by reference). Такой дуализм сделан с целью оптимизации скорости работы виртуальной машины.

   Работа с value типами в ol наиболее быстрая из всего, что может обеспечить виртуальная машина. Они не требуют размещения в памяти, математические операции с ними выполняются одной командой виртуальной машины (а не целой большой функцией) и, по большому счету, по скорости мало отличается от скорости операций на C или ассемблере.

   Как value можно использовать небольшие целые числа, которые укладываются в количество бит машинного слова платформы минус 8 бит для служебной информации. Это числа, не большие чем 16777215 (24 бита, #xFFFFFF, #o77777777) для 32-битных систем и 72057594037927935 (56 бит, #xFFFFFFFFFFFFFF) для 64-битных. Вполне достаточно для большинства прикладных задач. Такие числа в контексте ol называются fix числами и имеют типы type-fix+ и type-fix- для положительных и отрицательных, соответственно. 
   
   Числа, которые превышают эти значения автоматически разбиваются на пары по 24 и 56 бит соответственно и размещаются в куче. Такие числа в контексте ol называются int числами и имеют типы type-int+ и type-int- соответственно своему знаку. Детальнее о таких числах можно посмотреть в [базовом разделе](?ru/basic-concepts). 

   Далее приведен перечень команд виртуальной машины с опкодом для быстрой работы с fix числами, сопровождающийся примерами, так как многие из этих команд возвращают более одного значения. Следует заметить, что все эти команды работают с беззнаковыми value значениями и не проверяют типы своих аргументов. Таким образом, в случае, если операнды будут неправильного типа, то значение результата будет непредсказуемым (в debug версии виртуальной машины будет выборошено соответствующее исключение через assert).

* 38: (vm:add a b) - сложение a и b. возвращает пару чисел (values сумма переполнение)
<pre><code id="add" data-language="scheme">
(let* ((a carry (vm:add 4 3))) (print "a: " a ", carry: " carry))
; ==> a: 7, carry: #false

(let* ((a carry (vm:add 4 (fxmax)))) (print "a: " a ", carry: " carry))
; ==> a: 3, carry: #true
</code><button class="doit" onclick="doit(add.textContent)">отправить в терминал</button></pre>

* 40: (vm:sub a b) - вычитание b из a. возвращает пару чисел (values разница обратное-переполнение)
<pre><code id="sub" data-language="scheme">
(let* ((a carry (vm:sub 4 3))) (print "a: " a ", carry: " carry))
; ==> a: 1, carry: #false

(let* ((a carry (vm:sub 3 4))) (print "a: " a ", carry: " carry))
; ==> a: 72057594037927935, carry: #true    for 64-bit machines
; ==> a: 16777215, carry: #true             for 32-bit machines
</code><button class="doit" onclick="doit(sub.textContent)">отправить в терминал</button></pre>

* 39: (vm:mul a b) - умножение a на b. возвращает пару чисел (value lo-part hi-part) 
<pre><code id="mul" data-language="scheme">
(let* ((lo hi (vm:mul 4 3))) (print "lo: " lo ", hi: " hi))
; ==> lo: 12, hi: 0

(let* ((lo hi (vm:mul 16777215 3))) (print "lo: " lo ", hi: " hi))
; ...                                       for 64-bit machines
; ==> lo: 16777213, hi: 2                   for 32-bit machines

(let* ((lo hi (vm:mul 72057594037927935 7))) (print "lo: " lo ", hi: " hi))
; ...                                       for 64-bit machines
; ==> generated assertion                   for 32-bit machines
</code><button class="doit" onclick="doit(mul.textContent)">отправить в терминал</button></pre>

* 26: (vm:div ah al b) - деление a на b, где ah - старшая часть числа a, al - младшая. возвращает тройку чисел (values старшая-часть-частного младшая-часть-частного остаток)
<pre><code id="div" data-language="scheme">
(let* ((lo hi rem (vm:div 0 7 3))) (print "lo: " lo ", hi: " hi ", rem: " rem))
; ==> lo: 0, hi: 2, rem: 1
(let* ((lo hi rem (vm:div 2 16777213 3))) (print "lo: " lo ", hi: " hi ", rem: " rem))
; ==> lo: 0, hi: 16777215, rem: 0           for 32-bit machines
</code><button class="doit" onclick="doit(div.textContent)">отправить в терминал</button></pre>

* 58: (vm:shr a n) - битовый (логический) сдвиг вправо.
<pre><code id="shr" data-language="scheme">
</code><button class="doit" onclick="doit(shr.textContent)">отправить в терминал</button></pre>

* 59: (vm:shl a n) - битовый (логический) сдвиг влево.
<pre><code id="shl" data-language="scheme">
</code><button class="doit" onclick="doit(shl.textContent)">отправить в терминал</button></pre>

* 55: (vm:and a b) - операция дизъюнкции (И).
<pre><code id="and" data-language="scheme">
</code><button class="doit" onclick="doit(and.textContent)">отправить в терминал</button></pre>

* 56: (vm:or a b) - операция конъюнкции (ИЛИ).
<pre><code id="or" data-language="scheme">
</code><button class="doit" onclick="doit(or.textContent)">отправить в терминал</button></pre>

* 57: (vm:xor a b) - операция строгой дизъюнкции (исключающее ИЛИ).
<pre><code id="xor" data-language="scheme">
</code><button class="doit" onclick="doit(xor.textContent)">отправить в терминал</button></pre>

   В заключение хочется отметить, что не стоит без особой надобности использовать эти операции. Для выполнения математических и логических операций существует модуль (lang math), который экспортирует функции +, -, =, *, /, div, mod, \<<, <, <=, =, >=, >, \>>, band, bor, bxor, div, rem, mod, min, max, gcd и другие. 
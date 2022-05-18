Logic Wire
==========

I saw an idea at the [Bitmap Logic Simulator](https://realhet.wordpress.com/2015/09/02/bitmap-logic-simulator/).
And a bit simplified.

The Rules
=========
![](https://realhet.files.wordpress.com/2015/09/help.png)


Rule 1: use xpm file as source map
----------------------------------

Rule 2: zero color is a not-a-wire
----------------------------------

Rule 3: all other colors are wires
----------------------------------
We can use different colors to make circuit more understandable.

Rule 4: directly connected wires are the same wire
--------------------------------------------------

Rule 5: the intersection of wires through a not-a-wire is a wire crossing
-------------------------------------------------------------------------

Rule 6: the special pattern (including rotations) in a NOT gate
---------------------------------------------------------------

Rule 7: all NOT gates works simultaneously
------------------------------------------

Rule 8: wire is powered, if at least one power is connected to this wire
------------------------------------------------------------------------

Rule 9, additional: first wire color (a second, if counting from zero) initially powered
----------------------------------------------------------------------------------------

Thats all. Now we can generate schemes.


--------------
Триггер:
00 - запрещенное состояние
11 - сохраняет предыдущее 10 или 11, 00 и 11 являются запрещенными
10 - переключает в 01
01 - переключает в 10

Минимальная время переключения сигнала - 5 циклов. Если меньше, то триггер не успевает отреагировать. Этому соответствует блинкер из 5 NOT элементов.

-------------
Pinta Autokey

- black: mouse.click_relative(33-1,572-38,1)
- red: mouse.click_relative(33-1,604-38,1)

notes:
```
$ xdotool selectwindow
$ xdotool getwindowgeometry WINDOW-ID
show clicks: $ xinput test-xi2 --root

mouse.click_absolute(33,572,1)
mouse.click_relative(33-1,572-38,1)
```

-------------------------------------
Used Font
https://www.dafont.com/minecraft.font

-------------------------------------------------------------
Read Me
https://www.instructables.com/How-to-Build-an-8-Bit-Computer/
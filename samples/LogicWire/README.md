Logic Wire
==========

I saw an idea at the [Bitmap Logic Simulator](https://realhet.wordpress.com/2015/09/02/bitmap-logic-simulator/).
And a bit simplified.


The Rules
=========
![](https://i.imgur.com/yHLo71u.png)


Rule 1: directly connected cells are the same wire
--------------------------------------------------

Rule 2: the intersection of wires through a not-a-wire is a wire crossing
-------------------------------------------------------------------------

Rule 3: the special pattern (including rotations) is a NOT gate
---------------------------------------------------------------


Example
=======
![](https://i.imgur.com/DyKU2aj.gif)


Note 1: use xpm file as source map
----------------------------------
You can use png (`convert 8bit_cpu.xpm 8bit_cpu.png`), edit it with Pinta,
convert back to xpm (`convert 8bit_cpu.png 8bit_cpu.xpm`).

Note 2: zero color is a not-a-wire
----------------------------------

Note 3: all other colors are wires
----------------------------------
We can use different colors to make circuit more understandable.

Note 4: all NOT gates works simultaneously
------------------------------------------

Note 5: wire is powered, if at least one power is connected to this wire
------------------------------------------------------------------------

Note 6, additional: first wire color (a second, if counting from zero) initially powered
----------------------------------------------------------------------------------------

Thats all. Now we can generate schemes.


-------------------------------------------------------------
Credits:
--------

https://www.instructables.com/How-to-Build-an-8-Bit-Computer/

https://realhet.wordpress.com/2015/09/02/bitmap-logic-simulator/

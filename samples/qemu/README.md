Experimental eqmu/gdb Debug Interface
=====================================

[![Works over Otus Lisp](https://yuriy-chumak.github.io/ol/assets/view-project-page.svg)](https://yuriy-chumak.github.io/ol/)

VERSION 0.1

LICENSE
--------------------------------------------------------------

This program is free software;  you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 3 of
the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

You should have received a copy of the GNU GPL along with this
program. If not, see <http://www.gnu.org/licenses/>.


OVERVIEW
--------

QEmu command line can be changed in line that contains
"/usr/bin/qemu-system-i386".

Press [Enter] to execute command(s).


KEYBOARD SHORTCUTS
------------------

F2 - break execution
F9 - run (gdb 'continue')
F5 - step into (gdb 'si')
F6 - step over


FUNCTIONS
---------

You can use any lisp functions and primitives from embedded
command line (press [enter] to start), for example you can
type "(define x 123)" or "$ecx" or
"(define (f x) (if (eq? x 0) 1 (* x (f (- x 1)))))" with "(f 100)".

Variables $eax, $ebx, etc. automatically changed after every
execution break (step-into, step-over, stop, stc.). But (!)
please, this values have no backward propagation. To use
register values in functions prease use (eax), (ebx), and 
similar functions.

(step-into) - Step Into
(step-over) - Step Over
(stop) - Break Execution
(run) - Continue Execution

TBD.
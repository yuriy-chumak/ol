# OpenGL Tutorials

## OpenGL 1.x

We can include just base library and then include needed OpenGL version as
```scheme
(import (lib gl))
(gl:set-window-title "...")

(import (OpenGL 1.0))
```

Or we can include required version in one line as
```scheme
(import (lib gl 1.0))
(gl:set-window-title "...")
```

## OpenGL 2.x, 3.0

Same situation as in 1.x
```scheme
(import (lib gl))
(gl:set-window-title "...")

(import (OpenGL 2.1))
```
or
```scheme
(import (lib gl 2.1))
(gl:set-window-title "...")
```

## OpenGL 3.1+, 4.x

The situation is different from 2.x and 1.x, we need to explicitly include helper library:
```scheme
(import (lib gl 3.1))
(gl:set-window-title "...")
```
or
```scheme
(import (lib gl 3 context))
(gl:set-context-version 3 1)
(gl:set-window-title "...")

(import (OpenGL 3.1))
```


# Notes

Line Stipple:
https://stackoverflow.com/questions/6017176/gllinestipple-deprecated-in-opengl-3-1


; BTW, you can start ol session, execute this file using ',load "101. Creating an OpenGL Window.lisp"' command
; and still have interactive console (repl) while OpenGL window renders itself in background.
; Just type '(glClearColor 1 0 0 1)' and you will see that OpenGL window immediately will
; change the background color. Cool, yeah?

### nvidia opengl version debug
__GL_OverrideVersion=3.2 ./app
### amd opengl version debug
MESA_GL_VERSION_OVERRIDE=3.2 MESA_GLSL_VERSION_OVERRIDE=150 ./app

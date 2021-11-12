# 3D Game Shaders For Beginners

## Reference Frames

Please read the full article at [lettier/3d-game-shaders-for-beginners](https://github.com/lettier/3d-game-shaders-for-beginners/blob/master/sections/reference-frames.md).

## Otus Lisp notes

[4.reference-frames.lisp](../4.reference-frames.lisp):
```bash
$ ./4.reference-frames.lisp
```

![4.reference-frames.lisp screenshot](https://i.imgur.com/gT2o8vm.gif)

```scheme
(import (OpenGL version-2-1))
```

We use OpenGL 2.1 as a very simple and straightforward graphics library frontend. No preparations of index and vertex buffers, no calculations of buffers length, no many low-level technical moves - just begin, set a vertex, set a normal, end.


```scheme
(define models
(fold (lambda (models filename)
...
                                 (glNewList i GL_COMPILE)
                                 (glBegin GL_TRIANGLES)
                                 ...
                                 (glEnd)
                                 (glEndList)
   {}
   '("Mill" )))
```

We load the OBJ and the corresponded MTL wavefront geometry files and immediately compile the geometry as OpenGL lists.

Now we have a `models` that is a dictionary model-name->list-of-opengl-lists:
```json
{
   'Mill_Cube.128         '(1 2 3 4 5 6 7 8 9 10)
   'Mill_Blades_Cube.007  '(11 12 13 14)
}
```

```scheme
         (when (starts-with? model "Mill_Blades_Cube.")
            (glTranslatef 0 0 +3.1247)
            (let*((ss ms (uncons (syscall 96) #f))
                  (r (/ (mod (floor (* (+ ss (/ ms 1000000)) 700)) 36000) 100)))
               (glRotatef r 0 1 0))
            (glTranslatef 0 0 -3.1247))
```

A trick. We want to animate the scene so that the blades of the mill rotate.
I hardcoded the rotation for now and will update to a more elegant one later.

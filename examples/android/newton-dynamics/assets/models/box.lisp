; cube
(define x 0.5)
(define -x (- x))
(define vertices [
   [ x  x  x] ;1
   [ x  x -x] ;2
   [-x  x -x] ;3
   [-x  x  x] ;4
   
   [ x -x  x] ;5
   [ x -x -x] ;6
   [-x -x -x] ;7
   [-x -x  x] ;8
])

(define faces (list
   [1 2 3 4] ; top (yellow)
   [1 5 6 2] ; right (orange)
   [1 4 8 5] ; front (green)
   [4 3 7 8] ; red (left)
   [2 6 7 3] ; back (blue)
   [5 8 7 6] ; while (bottom)
))
(define colors (list
   '(1 1 0)
   '(1 0.31 0)
   '(0 1 0)
   '(1 0 0)
   '(0 0 1)
   '(1 1 1)
))
(define normals (list
   '(0 1 0)
   '(1 0 0)
   '(0 0 1)
   '(-1 0 0)
   '(0 -1 0)
   '(0 -1 0)
))

(define (draw-Box size1 size2 size3)
   (glPushMatrix)
   (glScalef size1 size2 size3)
   (glBegin GL_QUADS)
   (for-each (lambda (face color normal)
         (glColor3fv color)
         (glNormal3fv normal)
         (for-each (lambda (index)
               (glVertex3fv (ref vertices index)))
            face))
      faces colors normals)
   (glEnd)
   (glPopMatrix))

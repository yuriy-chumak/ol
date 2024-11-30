(glClearColor 0.2 0.2 0.2 1)

(define vertices [
   [ 1  1  1] ;1
   [ 1  1 -1] ;2
   [-1  1 -1] ;3
   [-1  1  1] ;4
   
   [ 1 -1  1] ;5
   [ 1 -1 -1] ;6
   [-1 -1 -1] ;7
   [-1 -1  1] ;8
])
(define indices (list
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

(define (cube:draw)
   (glBegin GL_QUADS)
   (for-each (lambda (index color)
         (glColor3fv color)
         (glVertex3fv (ref vertices (ref index 1)))
         (glVertex3fv (ref vertices (ref index 2)))
         (glVertex3fv (ref vertices (ref index 3)))
         (glVertex3fv (ref vertices (ref index 4))) )
      indices colors)
   (glEnd))

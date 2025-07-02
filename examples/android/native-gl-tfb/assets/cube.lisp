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

(define (cube:draw)
   (for-each (lambda (index tex)
         (glBindTexture GL_TEXTURE_2D tex)
         (glBegin GL_QUADS)
            (glTexCoord2f 0 0)
            (glVertex3fv (ref vertices (ref index 1)))
            (glTexCoord2f 0 1)
            (glVertex3fv (ref vertices (ref index 2)))
            (glTexCoord2f 1 1)
            (glVertex3fv (ref vertices (ref index 3)))
            (glTexCoord2f 1 0)
            (glVertex3fv (ref vertices (ref index 4)))
         (glEnd) )
      indices textures))

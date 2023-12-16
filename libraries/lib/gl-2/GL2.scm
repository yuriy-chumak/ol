(import (OpenGL EXT geometry_shader4))

(define (vertex-preprocessor source)
   (if (list? source) source (list source)))
(define (geometry-preprocessor source inputType outputType outputCount)
   (define shader (if (list? source) (apply string-append source) source))
   (define output (list
      (if (m/#version +120/ shader) "\n" "")
      ;;"#extension GL_EXT_geometry_shader4: enable\n"
      "\n"
      (fold (lambda (source regex)
               (regex source))
         shader (list
            s/#version +120//
         ))))
   output)
(define (fragment-preprocessor source)
   (if (list? source) source (list source)))


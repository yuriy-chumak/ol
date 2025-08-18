(define-library (lib ansi)
(export
   RED
   GREEN
   YELLOW
   BLUE
   MAGENTA
   CYAN
   WHITE
   END
)

(import (scheme core))

(begin
   (define RED "\e[0;31m")
   (define GREEN "\e[0;32m")
   (define YELLOW "\e[0;33m")
   (define BLUE "\e[0;34m")
   (define MAGENTA "\e[0;35m")
   (define CYAN "\e[0;36m")
   (define WHITE "\e[0;37m")
   (define END "\e[0;0m")
))

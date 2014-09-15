                             ; OpenGL 3.0 (2008)

; todo: ввести понятие "Deprecation Model"
;  а значит ввести набор переменных xxx_DEPRECATED, которые должны использоваться
;  в классах младших версий opengl и выключать не используемые (устаревшие) фичи.
;  для этого надо добавить в OL примитив (defined?), который я там пока не нашел.

(import         (OpenGL version-2-1))
(define-library (OpenGL version-3-0)
  (export
    GL_VERSION_3_0

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_3_0    1)
(define % (dlopen "opengl32" 0))

))
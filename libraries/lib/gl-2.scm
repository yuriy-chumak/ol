(define-library (lib gl-2)
(import
   (scheme core)
   (scheme bytevector)
   (lib gl config)
   (lib gl)
   (OpenGL version-2-1))
(export
   gl:create-program

   (exports (lib gl))
   (exports (OpenGL version-2-1)))
   
(begin

   (define (compile shader sources)
      (glShaderSource shader (length sources) sources #false)
      (glCompileShader shader)
      (let ((isCompiled (box 0)))
         (glGetShaderiv shader GL_COMPILE_STATUS isCompiled)

         (if (eq? (unbox isCompiled) 0)
            (let*((maxLength (box 0))
                  (_ (glGetShaderiv shader GL_INFO_LOG_LENGTH maxLength))
                  (maxLengthValue (unbox maxLength))
                  (errorLog (make-bytevector maxLengthValue 0))
                  (_ (glGetShaderInfoLog shader maxLengthValue maxLength errorLog)))
               (raise (utf8->string errorLog))))))

   (define (link program . shaders)
      (for-each (lambda (shader)
            (glAttachShader program shader))
         shaders)

      (glLinkProgram program)
      (let ((isLinked (box 0)))
         (glGetProgramiv program GL_LINK_STATUS isLinked)
         (if (eq? (unbox isLinked) 0)
            ;; the maxLength includes the NULL character
            (let*((maxLength (box 0))
                  (_ (glGetProgramiv program GL_INFO_LOG_LENGTH maxLength))
                  (maxLengthValue (unbox maxLength))
                  (errorLog (make-bytevector maxLengthValue 0))
                  (_ (glGetProgramInfoLog program maxLengthValue maxLength errorLog)))

               ;; we don't need the program anymore.
               (glDeleteProgram program)
               ;; don't leak shaders either.
               (for-each (lambda (shader)
                     (glDeleteShader shader))
                  shaders)
               ;; throw error
               (raise (utf8->string errorLog)))))

      (for-each (lambda (shader)
            ;; always detach shaders after a successful link.
            (glDetachShader program shader))
         shaders))

   (import (OpenGL EXT geometry_shader4))
   (define gl:create-program (case-lambda
      ((vstext fstext)
               (let ((po (glCreateProgram))
                     (vs (glCreateShader GL_VERTEX_SHADER))
                     (fs (glCreateShader GL_FRAGMENT_SHADER)))
                  (if (eq? po 0)
                     (raise "Can't create shader program."))

                  (compile vs (if (list? vstext) vstext (list vstext)))
                  (compile fs (if (list? fstext) fstext (list fstext)))

                  (link po vs fs)
                  po))
      ((vstext
        inputType outputType outputCount gstext
        fstext)
               (let ((program (glCreateProgram))
                     (gs (glCreateShader GL_GEOMETRY_SHADER)) ; (OpenGL EXT geometry_shader4)
                     (vs (glCreateShader GL_VERTEX_SHADER))
                     (fs (glCreateShader GL_FRAGMENT_SHADER)))
                  (if (eq? program 0)
                     (raise "Can't create shader program."))

                  (apply compile gs (if (list? gstext) gstext (list gstext)))
                  (apply compile vs (if (list? vstext) vstext (list vstext)))
                  (apply compile fs (if (list? fstext) fstext (list fstext)))

                  (glProgramParameteri program GL_GEOMETRY_INPUT_TYPE inputType)
                  (glProgramParameteri program GL_GEOMETRY_OUTPUT_TYPE outputType) ; only POINTS, LINE_STRIP and TRIANGLE_STRIP is allowed:
                  (glProgramParameteri program GL_GEOMETRY_VERTICES_OUT outputCount)

                  (link program gs vs fs)
                  program))))

))

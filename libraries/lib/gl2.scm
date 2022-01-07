(define-library (lib gl2)
(import
   (scheme core)
   (scheme bytevector)
   (lib gl) (OpenGL version-2-1))
(export
   gl:create-program

   (exports (lib gl))
   (exports (OpenGL version-2-1)))
   
(begin
   (import (owl io))
   (print-to stderr "(lib gl2) is deprecated. Use (lib gl-2) instead!")

   (define (compile shader source)
      (glShaderSource shader 1 (list source) #false)
      (glCompileShader shader)
      (let ((isCompiled (box 0)))
         (glGetShaderiv shader GL_COMPILE_STATUS isCompiled)

         (if (eq? (unbox isCompiled) 0)
            (let*((maxLength (box 0))
                  (_ (glGetShaderiv shader GL_INFO_LOG_LENGTH maxLength))
                  (maxLengthValue (unbox maxLength))
                  (errorLog (make-bytevector maxLengthValue 0))
                  (_ (glGetShaderInfoLog shader maxLengthValue maxLength errorLog)))
               (runtime-error (utf8->string errorLog) shader)))))

   (define (link program . shaders)
      (for-each (lambda (shader)
            (glAttachShader program shader))
         shaders)

      (glLinkProgram program)
      (let ((isLinked (box 0)))
         (glGetProgramiv program GL_LINK_STATUS isLinked)
         (if (eq? (unbox isLinked) 0)
            (let*((maxLength (box 0))
                  (_ (glGetProgramiv program GL_INFO_LOG_LENGTH maxLength))
                  (maxLengthValue (unbox maxLength))
                  (errorLog (make-bytevector maxLengthValue 0))
                  (_ (glGetProgramInfoLog program maxLengthValue maxLength errorLog)))
               (runtime-error (utf8->string errorLog) program))))

      (for-each (lambda (shader)
            (glDetachShader program shader))
         shaders))

   (import (OpenGL EXT geometry_shader4))
   (define gl:create-program (case-lambda
      ((vstext fstext)
               (let ((po (glCreateProgram))
                     (vs (glCreateShader GL_VERTEX_SHADER))
                     (fs (glCreateShader GL_FRAGMENT_SHADER)))
                  (if (eq? po 0)
                     (runtime-error "Can't create shader program." #f))

                  (compile vs vstext)
                  (compile fs fstext)

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
                     (runtime-error "Can't create shader program." #f))

                  (compile gs gstext)
                  (compile vs vstext)
                  (compile fs fstext)

                  (glProgramParameteri program GL_GEOMETRY_INPUT_TYPE inputType)
                  (glProgramParameteri program GL_GEOMETRY_OUTPUT_TYPE outputType) ; only POINTS, LINE_STRIP and TRIANGLE_STRIP is allowed
                  (glProgramParameteri program GL_GEOMETRY_VERTICES_OUT outputCount)

                  (link program gs vs fs)
                  program))))

))

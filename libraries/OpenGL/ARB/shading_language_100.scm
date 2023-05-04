; ===========================================================================
; ARB_shading_language_100                           (included in OpenGL 2.0)
;
;	https://registry.khronos.org/OpenGL/extensions/ARB/ARB_shading_language_100.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shading_language_100)

(import (scheme base)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL ARB shader_objects)
   (OpenGL ARB fragment_shader)
   (OpenGL ARB vertex_shader))

; ---------------------------------------------------------------------------
(export ARB_shading_language_100

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_SHADING_LANGUAGE_VERSION_ARB

   gl:create-program
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_shading_language_100 (gl:QueryExtension "GL_ARB_shading_language_100"))

   (define GL_SHADING_LANGUAGE_VERSION_ARB #x8B8C)

   ; * ol internal
   (define (compile shader sources)
      (glShaderSourceARB shader (length sources) sources #false)
      (glCompileShaderARB shader)
      (let ((isCompiled (box 0)))
         (glGetObjectParameterivARB shader GL_OBJECT_COMPILE_STATUS_ARB isCompiled)

         (if (eq? (unbox isCompiled) 0)
            (let*((maxLength (box 0))
                  (_ (glGetObjectParameterivARB shader GL_OBJECT_INFO_LOG_LENGTH_ARB maxLength))
                  (maxLengthValue (unbox maxLength))
                  (errorLog (make-bytevector maxLengthValue 0))
                  (_ (glGetInfoLogARB shader maxLengthValue maxLength errorLog)))
               (raise (utf8->string errorLog))))))

   (define (link program . shaders)
      (for-each (lambda (shader)
            (glAttachObjectARB program shader))
         shaders)
      (for-each (lambda (shader)
            (glDeleteObjectARB shader))
         shaders)

      (glLinkProgramARB program)
      (let ((isLinked (box 0)))
         (glGetObjectParameterivARB program GL_OBJECT_LINK_STATUS_ARB isLinked)
         (if (eq? (unbox isLinked) 0)
            ;; the maxLength includes the NULL character
            (let*((maxLength (box 0))
                  (_ (glGetObjectParameterivARB program GL_OBJECT_INFO_LOG_LENGTH_ARB maxLength))
                  (maxLengthValue (unbox maxLength))
                  (errorLog (make-bytevector maxLengthValue 0))
                  (_ (glGetInfoLogARB program maxLengthValue maxLength errorLog)))

               ;; we don't need the program anymore.
               (glDeleteObjectARB program)
               ;; throw error
               (raise (utf8->string errorLog)))))

      (for-each (lambda (shader)
            ;; always detach shaders after a successful link.
            (glDetachObjectARB program shader))
         shaders))

   (define (gl:create-program vstext fstext)
      (let ((po (glCreateProgramObjectARB))
            (vs (glCreateShaderObjectARB GL_VERTEX_SHADER_ARB))
            (fs (glCreateShaderObjectARB GL_FRAGMENT_SHADER_ARB)))
         (if (eq? po 0)
            (raise "Can't create shader program."))

         (compile vs (list vstext))
         (compile fs (list fstext))

         (link po vs fs)
         po))

))

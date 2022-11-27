(define-library (lib gl-2)
(import
   (scheme base)
   (lib gl config)
   (lib gl)
   (OpenGL version-2-1))
(export
   gl:create-program

   (exports (lib gl))
   (exports (OpenGL version-2-1)))

; todo: move "#version" from the source to the top
; note:
;  no GL_VERTEX_PROGRAM_TWO_SIDE
(cond-expand
   (Android
      (import (OpenGL EXT geometry_shader))
      (begin
         (import (owl io))
         (define (vertex-preprocessor source)
            (define shader (if (list? source) (apply string-append source) source))
            (define output (list
               (if (m/#version +120/ shader) "#version 100\n" "")
               "#define GL2ES 1\n"
               "precision highp float;"
               ; attributes
               (if (m/gl_Vertex/ shader) "attribute vec4 g2_Vertex;" "")
               (if (m/gl_Color/ shader) "attribute vec4 g2_Color;" "")
               (if (m/gl_SecondaryColor/ shader) "attribute vec4 g2_SecondaryColor;" "")
               (if (m/gl_Normal/ shader) "attribute vec3 g2_Normal;" "")
               (if (m/gl_MultiTexCoord0/ shader) "attribute vec4 g2_MultiTexCoord0;" "")
               ;; (if (m/gl_MultiTexCoord1/ shader) "attribute vec4 g2_MultiTexCoord1;" "")
               ;; (if (m/gl_MultiTexCoord2/ shader) "attribute vec4 g2_MultiTexCoord2;" "")
               ;; (if (m/gl_MultiTexCoord3/ shader) "attribute vec4 g2_MultiTexCoord3;" "")
               ;; (if (m/gl_MultiTexCoord4/ shader) "attribute vec4 g2_MultiTexCoord4;" "")
               ;; (if (m/gl_MultiTexCoord5/ shader) "attribute vec4 g2_MultiTexCoord5;" "")
               ;; (if (m/gl_MultiTexCoord6/ shader) "attribute vec4 g2_MultiTexCoord6;" "")
               ;; (if (m/gl_MultiTexCoord7/ shader) "attribute vec4 g2_MultiTexCoord7;" "")
               (if (m/gl_FogCoord/ shader)       "attribute vec4 g2_FogCoord;" "")
               ; uniforms
               (if (m/gl_ModelViewMatrix/ shader)           "uniform mat4 g2_ModelViewMatrix;" "")
               (if (m/gl_ModelViewProjectionMatrix/ shader) "uniform mat4 g2_ModelViewProjectionMatrix;" "")
               (if (m/gl_ProjectionMatrix/ shader)          "uniform mat4 g2_ProjectionMatrix;" "")
               (if (m/gl_TextureMatrix/ shader)             "uniform mat4 g2_TextureMatrix[8];" "")
               ;; todo: ...Inverse, ...Transpose; ...InverseTranspose
               (if (m/gl_NormalMatrix/ shader)              "uniform mat4 g2_NormalMatrix;" "")
               ;; todo: (if (m/gl_LightSource/ shader) "struct g2_LightSourceParameters {
               ;;    // ...
               ;;    vec4 position;
               ;;    // ...
               ;; };
               ;; uniform g2_LightSourceParameters g2_LightSource[2];" "")
               ; output
               (if (m/gl_FrontColor/ shader) "varying vec4 g2_FrontColor;" "")
               (if (m/gl_BackColor/ shader) "varying vec4 g2_BackColor;" "")
               (if (m/gl_FrontSecondaryColor/ shader) "varying vec4 g2_FrontSecondaryColor;" "")
               (if (m/gl_TexCoord/ shader) "varying vec4 g2_TexCoord[1];" "") ; 1, not 8
               (if (m/gl_FogFragCoord/ shader) "varying vec4 g2_FogFragCoord;" "")
               "\n"
               (fold (lambda (source regex)
                        (regex source))
                  shader (list
                     s/#version +120//
                     ; input
                     s/gl_Vertex/g2_Vertex/g
                     s/gl_Color/g2_Color/g
                     s/gl_SecondaryColor/g2_SecondaryColor/g
                     s/gl_Normal/g2_Normal/g
                     s/gl_MultiTexCoord0/g2_MultiTexCoord0/g
                     ;; s/gl_MultiTexCoord1/g2_MultiTexCoord1/g
                     ;; s/gl_MultiTexCoord2/g2_MultiTexCoord2/g
                     ;; s/gl_MultiTexCoord3/g2_MultiTexCoord3/g
                     ;; s/gl_MultiTexCoord4/g2_MultiTexCoord4/g
                     ;; s/gl_MultiTexCoord5/g2_MultiTexCoord5/g
                     ;; s/gl_MultiTexCoord6/g2_MultiTexCoord6/g
                     ;; s/gl_MultiTexCoord7/g2_MultiTexCoord7/g
                     s/gl_FogCoord/g2_FogCoord/g
                     ; uniforms
                     s/gl_ModelViewMatrix/g2_ModelViewMatrix/g
                     s/gl_ModelViewProjectionMatrix/g2_ModelViewProjectionMatrix/g
                     s/gl_ProjectionMatrix/g2_ProjectionMatrix/g
                     s/gl_TextureMatrix/g2_TextureMatrix/g
                     s/gl_NormalMatrix/g2_NormalMatrix/g
                     ;; s/gl_LightSource/g2_LightSource/g
                     ; output
                     s/gl_FrontColor/g2_FrontColor/g
                     s/gl_BackColor/g2_BackColor/g
                     s/gl_TexCoord/g2_TexCoord/g
                  ))))
            ;; (print output)
            output)

         ;; no version in geometry shader?
         (define (geometry-preprocessor source inputType outputType outputCount)
            (define shader (if (list? source) (apply string-append source) source))
            (define output (list
               (if (m/#version +120/ shader) "\n" "")
               "#extension GL_EXT_geometry_shader: enable\n"
               "#define GL2ES 1\n"
               ; attributes
               "\n"
               (fold (lambda (source regex)
                        (regex source))
                  shader (list
                     s/#version +120//
                     ; input
                     ; uniforms
                     ; output
                     ;...
                  ))))
            ;; (print output)
            output)

         (define (fragment-preprocessor source)
            (define shader (if (list? source) (apply string-append source) source))
            (define output (list
               (if (m/#version +120/ shader) "#version 100\n" "")
               "#define GL2ES 1\n"
               "precision highp float;" ; mediump в 3d нам портит картинку
               ; attributes
               (if (m/gl_Color/ shader) "varying vec4 g2_FrontColor;" "")
               (if (m/gl_SecondaryColor/ shader) "varying vec4 g2_SecondaryColor;" "")
               (if (m/gl_TexCoord/ shader) "varying vec4 g2_TexCoord[1];" "") ; 1, not 8
               (if (m/gl_FogFragCoord/ shader) "varying vec4 g2_FogFragCoord;" "")
               ; uniforms
               ;; (if (m/gl_LightSource/ shader) "struct g2_LightSourceParameters {
               ;;    // ...
               ;;    vec4 position;
               ;;    // ...
               ;; };
               ;; uniform g2_LightSourceParameters g2_LightSource[2];" "")
               ; output
               "\n"
               (fold (lambda (source regex)
                        (regex source))
                  shader (list
                     s/#version +120//
                     ; input
                     s/gl_Color/g2_FrontColor/g
                     s/gl_SecondaryColor/g2_FrontSecondaryColor/g
                     s/gl_TexCoord/g2_TexCoord/g
                     s/gl_FogFragCoord/g2_FogFragCoord/g
                     ; uniforms
                     ;; s/gl_LightSource/g2_LightSource/g
                     ; output
                     ;...
                  ))))
            ;; (print output)
            output)

         ; no geometry shaders support yet
         (define (glProgramParameteri . args) #false)
         (define GL_GEOMETRY_INPUT_TYPE 0)
         (define GL_GEOMETRY_OUTPUT_TYPE 0)
         (define GL_GEOMETRY_VERTICES_OUT 0)

         #true))
   (else
      (import (OpenGL EXT geometry_shader4))
      (begin
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
            (if (list? source) source (list source))) )))

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

   (define gl:create-program (case-lambda
      ((vstext fstext)
               (let ((po (glCreateProgram))
                     (vs (glCreateShader GL_VERTEX_SHADER))
                     (fs (glCreateShader GL_FRAGMENT_SHADER)))
                  (if (eq? po 0)
                     (raise "Can't create shader program."))

                  (compile vs (vertex-preprocessor vstext))
                  (compile fs (fragment-preprocessor fstext))

                  (link po vs fs)
                  po))
      ((vstext
        inputType outputType outputCount gstext
        fstext)
               (let ((program (glCreateProgram))
                     (gs (glCreateShader GL_GEOMETRY_SHADER))
                     (vs (glCreateShader GL_VERTEX_SHADER))
                     (fs (glCreateShader GL_FRAGMENT_SHADER)))
                  (if (eq? program 0)
                     (raise "Can't create shader program."))

                  (compile vs (vertex-preprocessor vstext))
                  (compile gs (geometry-preprocessor gstext inputType outputType outputCount))
                  (compile fs (fragment-preprocessor fstext))

                  (glProgramParameteri program GL_GEOMETRY_INPUT_TYPE inputType)
                  (glProgramParameteri program GL_GEOMETRY_OUTPUT_TYPE outputType) ; only POINTS, LINE_STRIP and TRIANGLE_STRIP is allowed:
                  (glProgramParameteri program GL_GEOMETRY_VERTICES_OUT outputCount)

                  (link program gs vs fs)
                  program))))

))

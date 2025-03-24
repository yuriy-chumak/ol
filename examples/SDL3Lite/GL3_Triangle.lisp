#!/usr/bin/env ol

(import (lib sdl3lite))
(import (OpenGL 3.3))

; -- main ----------------
(define WINDOW_WIDTH  640)
(define WINDOW_HEIGTH 480)

(unless (SDL_Init SDL_INIT_VIDEO)
   (SDL_Log "Init error: %s\n" (SDL_GetError))
   (exit 1))

(SDL_GL_SetAttribute SDL_GL_CONTEXT_MAJOR_VERSION 3)
(SDL_GL_SetAttribute SDL_GL_CONTEXT_MINOR_VERSION 3)

(define window (SDL_CreateWindow "OpenGL3" WINDOW_WIDTH WINDOW_HEIGTH SDL_WINDOW_OPENGL))
(unless window
   (SDL_Log "Create window error: %s\n" (SDL_GetError))
   (exit 1))

(define context (SDL_GL_CreateContext window))
(unless context
   (SDL_Log "Create context error: %s\n" (SDL_GetError))
   (exit 1))

(glViewport 0 0 WINDOW_WIDTH WINDOW_HEIGTH)

(define vertexShader (glCreateShader GL_VERTEX_SHADER))
(glShaderSource vertexShader 1 (list
"  #version 330 core
   layout (location = 0) in vec3 aPos;
   void main()
   {
      gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
   }") #f)
(glCompileShader vertexShader)

(define fragmentShader (glCreateShader GL_FRAGMENT_SHADER))
(glShaderSource fragmentShader 1 (list
"  #version 330 core
   out vec4 FragColor;
   void main()
   {
      FragColor = vec4(0.8f, 0.3f, 0.02f, 1.0f);
   }") #f)
(glCompileShader fragmentShader)

(define shaderProgram (glCreateProgram))
(glAttachShader shaderProgram vertexShader)
(glAttachShader shaderProgram fragmentShader)
(glLinkProgram shaderProgram)

(glDeleteShader vertexShader)
(glDeleteShader fragmentShader)

(define vertices [
  -0.5  (/ (* -0.5 (sqrt 3)  ) 3)  0.0
   0.5  (/ (* -0.5 (sqrt 3)  ) 3)  0.0
   0.0  (/ (*  0.5 (sqrt 3) 2) 3)  0.0
])

(define &VAO (box 0))
(glGenVertexArrays 1 &VAO)
(define VAO (unbox &VAO))

(define &VBO (box 0))
(glGenBuffers 1 &VBO)
(define VBO (unbox &VBO))

(glBindVertexArray VAO)
(glBindBuffer GL_ARRAY_BUFFER VBO)
(glBufferData GL_ARRAY_BUFFER (* (size vertices) 4) (cons GLfloat* vertices) GL_STATIC_DRAW)
(glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE (* 3 4) #f)
(glEnableVertexAttribArray 0)
(glBindBuffer GL_ARRAY_BUFFER 0)
(glBindVertexArray 0)

(let loop ()
   (define event (make-SDL_Event))
   (define done (let loop ((done #false))
      (if (= (SDL_PollEvent event) 0)
         done
         (loop (or done (= (SDL_Event->type event) SDL_EVENT_QUIT))))))

   (glClearColor 0.07 0.13 0.17 1.0)
   (glClear GL_COLOR_BUFFER_BIT)
   (glUseProgram shaderProgram)
   (glBindVertexArray VAO)
   (glDrawArrays GL_TRIANGLES 0 3)

   (SDL_GL_SwapWindow window)
   (unless done (loop)))

(glDeleteVertexArrays 1 &VAO)
(glDeleteBuffers 1 &VBO)
(glDeleteProgram shaderProgram)

(SDL_GL_DestroyContext context)
(SDL_DestroyWindow window)
(SDL_Quit)

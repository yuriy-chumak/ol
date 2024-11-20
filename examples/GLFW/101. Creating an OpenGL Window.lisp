#!/usr/bin/env ol

(import (lib glfw))

(glfwSetErrorCallback
   (GLFWerrorfun (error_code description)
      (print error_code ": " description)))

; init
(unless (glfwInit)
   (runtime-error "glfw error"))

(glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 2)
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)
(define window (glfwCreateWindow 640 480 "The Title" NULL NULL))

; the OpenGL
(glfwMakeContextCurrent window)
(import (OpenGL 1.1))

; draw
(let loop ()
   (unless (glfwWindowShouldClose window)
      (glClearColor 0.3 0.3 0.3 1)
      (glClear GL_COLOR_BUFFER_BIT)

      (glBegin GL_TRIANGLES)
         (glColor3f 1 0 0)
         (glVertex2f -0.6 -0.6)

         (glColor3f 0 1 0)
         (glVertex2f +0.6 -0.6)

         (glColor3f 0 0 1)
         (glVertex2f -0.0 +0.7)
      (glEnd)

      (glfwSwapBuffers window)
      (glfwPollEvents) (sleep)
      (loop)))

; done
(glfwDestroyWindow window)
(glfwTerminate)

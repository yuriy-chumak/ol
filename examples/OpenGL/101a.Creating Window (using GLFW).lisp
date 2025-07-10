#!/usr/bin/env ol

;import GLFW libary
(import (lib glfw))
(glfwInit)

; create OpenGL 1.0 window
(glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 1)
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)
(define window (glfwCreateWindow 1280 720 "1. Creating an OpenGL Window (using GLFW)" NULL NULL))

; activate OpenGL context
(glfwMakeContextCurrent window)

; import OpenGL functions
(import (OpenGL 1.1))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render loop
(let loop ()
   (unless (glfwWindowShouldClose window)
      (glClear GL_COLOR_BUFFER_BIT)

      ; finish render pass
      (glfwSwapBuffers window)
      (glfwPollEvents) (sleep)
      (loop)))

; cleanup
(glfwDestroyWindow window)
(glfwTerminate)

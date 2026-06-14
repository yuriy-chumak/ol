#!/usr/bin/env ol

;import GLFW libary
(import (lib glfw))
(glfwInit)

; create OpenGL window
(glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 1)
(glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 0)
(define window (glfwCreateWindow 1280 720 "1. Creating an OpenGL Window (using GLFW)" NULL NULL))

; activate OpenGL context
(glfwMakeContextCurrent window)

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render loop
(do ()
    ((glfwWindowShouldClose window) #f)
   (glClear GL_COLOR_BUFFER_BIT)

   ; finish render pass
   (glfwSwapBuffers window)
   (glfwPollEvents) (sleep))

; cleanup
(glfwDestroyWindow window)
(glfwTerminate)

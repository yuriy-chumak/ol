(define-library (lib glfw)
   (import (otus lisp)
           (otus ffi))
   (export
      glfwInit
      glfwTerminate
      GLFWerrorfun |make GLFWerrorfun|
      glfwSetErrorCallback

      glfwWindowHint
         GLFW_CONTEXT_VERSION_MAJOR
         GLFW_CONTEXT_VERSION_MINOR
      glfwCreateWindow
      glfwDestroyWindow
      glfwWindowShouldClose
      glfwGetWindowSize

      glfwPollEvents

      glfwMakeContextCurrent
      glfwSwapBuffers

      ; (otus ffi)
      NULL
   )

(cond-expand
   (Windows
      (begin
         (define libglfw (or (load-dynamic-library "glfw3.dll")
                             (runtime-error "Can't load libglfw"
                                      "go to https://www.glfw.org/download.html and download one")))))
   (Android
      (begin
         (define libglfw (or (load-dynamic-library "libglfw.so")
                             (load-dynamic-library "libglfw.so.3")
                             (runtime-error "Can't load libglfw"
                                      "try to rebuild apk")))))
   (else
      (begin
         (define libglfw (or (load-dynamic-library "libglfw.so")
                             (load-dynamic-library "libglfw.so.3")
                             (runtime-error "Can't load libglfw"
                                      "try to install 'libglfw3' package, or\n"
                               "       check the libglfw homepage at https://www.glfw.org/download.html"))))) )

(begin
   (setq GLFW libglfw)

   (setq int fft-int)
   (setq int& fft-int&)
   (setq void fft-void)
   (setq bool fft-bool)

   (setq GLFWmonitor* type-vptr)
   (setq GLFWwindow* type-vptr)

   (define glfwInit (GLFW int "glfwInit"))
   (define glfwTerminate (GLFW void "glfwTerminate"))
   ;glfwInitHint
   ;glfwGetVersion
   ;glfwGetVersionString
   ;glfwGetError

   (define (|make GLFWerrorfun| handler)
      (make-callback
         (vm:pin (cons
            (cons type-vptr (list fft-int type-string))
            handler
         ))))
   (define-syntax GLFWerrorfun
      (syntax-rules ()
         ((GLFWerrorfun (code description) . body)
            (|make GLFWerrorfun|
               (lambda (code description) . body) ))))

   (define GLFWerrorfun* type-callable)
   (define glfwSetErrorCallback (GLFW GLFWerrorfun* "glfwSetErrorCallback" GLFWerrorfun*))

   ;glfwGetMonitors
   ;glfwGetPrimaryMonitor
   ;glfwGetMonitorPos
   ;glfwGetMonitorWorkarea
   ;glfwGetMonitorPhysicalSize
   ;glfwGetMonitorContentScale
   ;glfwGetMonitorName
   ;glfwSetMonitorUserPointer
   ;glfwGetMonitorUserPointer
   ;glfwSetMonitorCallback

   ;glfwGetVideoModes
   ;glfwGetVideoMode
   ;glfwSetGamma
   ;glfwGetGammaRamp
   ;glfwSetGammaRamp

   ;glfwDefaultWindowHints
   (define glfwWindowHint (GLFW void "glfwWindowHint" int int))
      (setq GLFW_CONTEXT_VERSION_MAJOR  #x00022002)
      (setq GLFW_CONTEXT_VERSION_MINOR  #x00022003)
   ;glfwWindowHintString
   (define glfwCreateWindow (GLFW GLFWwindow* "glfwCreateWindow" int int type-string GLFWmonitor* GLFWwindow*))
   (define glfwDestroyWindow (GLFW void "glfwDestroyWindow" GLFWwindow*))
   (define glfwWindowShouldClose (GLFW bool "glfwWindowShouldClose" GLFWwindow*))

   (define glfwGetWindowSize (GLFW void "glfwGetWindowSize" GLFWwindow* int& int&))

   (define glfwPollEvents (GLFW void "glfwPollEvents"))

   (define glfwMakeContextCurrent (GLFW void "glfwMakeContextCurrent" GLFWwindow*))
   ;glfwGetCurrentContext
   (define glfwSwapBuffers (GLFW void "glfwSwapBuffers" GLFWwindow*))
   ;glfwSwapInterval
))

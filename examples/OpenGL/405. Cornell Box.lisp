#!/usr/bin/env ol
(import (lib gl 4.6 core))
(gl:set-window-title "2. Drawing a Point")

; let's check context version
(define major (box 0))
(define minor (box 0))
(glGetIntegerv GL_MAJOR_VERSION major)
(glGetIntegerv GL_MINOR_VERSION minor)
(print "Context version: " (unbox major) "." (unbox minor))
(print "OpenGL version: " (glGetString GL_VERSION))

; init
(glEnable GL_DEPTH_TEST)
(glClearColor 0.3 0.3 0.3 1)

; --------
(define Vertices '(
   ( 0        0       0          0    0    0                     0   0    0 0 0) ; dummy point
      ;; floor
   ( 0.0      0.0    -0.5592     0.0  1.0  0.0                   1.0 1.0  1 1 1)
   ( 0.0      0.0     0.0        0.0  1.0  0.0                   1.0 0.0  1 1 1)
   (-0.5528   0.0     0.0        0.0  1.0  0.0                   0.0 0.0  1 1 1)
   (-0.5496   0.0    -0.5592     0.0  1.0  0.0                   0.0 1.0  1 1 1)
      ;; back wall
   ( 0.0      0.0    -0.5592     0.0  0.0  1.0                   0.0 0.0  1 1 1)
   (-0.5496   0.0    -0.5592     0.0  0.0  1.0                   0.0 0.0  1 1 1)
   (-0.556    0.5488 -0.5592     0.0  0.0  1.0                   0.0 0.0  1 1 1)
   ( 0.0      0.5488 -0.5592     0.0  0.0  1.0                   0.0 0.0  1 1 1)
      ;; ceiling
   (-0.556    0.5488 -0.5592     0.0 -1.0  0.0                   0.0 0.0  1 1 1)
   ( 0.0      0.5488 -0.5592     0.0 -1.0  0.0                   0.0 0.0  1 1 1)
   ( 0.0      0.5488  0.0        0.0 -1.0  0.0                   0.0 0.0  1 1 1)
   (-0.556    0.5488  0.0        0.0 -1.0  0.0                   0.0 0.0  1 1 1)
      ;; left wall
   (-0.5528   0.0     0.0        1.0  0.0  0.0                   0.0 0.0  1 0 0)
   (-0.5496   0.0    -0.5592     1.0  0.0  0.0                   0.0 0.0  1 0 0)
   (-0.556    0.5488  0.0        1.0  0.0  0.0                   0.0 0.0  1 0 0)
   (-0.556    0.5488 -0.5592     1.0  0.0  0.0                   0.0 0.0  1 0 0)
      ;; right wall
   ( 0.0      0.0    -0.5592    -1.0  0.0  0.0                   0.0 0.0  0 1 0)
   ( 0.0      0.0     0.0       -1.0  0.0  0.0                   0.0 0.0  0 1 0)
   ( 0.0      0.5488 -0.5592    -1.0  0.0  0.0                   0.0 0.0  0 1 0)
   ( 0.0      0.5488  0.0       -1.0  0.0  0.0                   0.0 0.0  0 1 0)
      ;; light
   (-0.213    0.5478 -0.227      0.0 -1.0  0.0                   0.0 0.0  1 1 0)
   (-0.343    0.5478 -0.227      0.0 -1.0  0.0                   0.0 0.0  1 1 0)
   (-0.343    0.5478 -0.332      0.0 -1.0  0.0                   0.0 0.0  1 1 0)
   (-0.213    0.5478 -0.332      0.0 -1.0  0.0                   0.0 0.0  1 1 0)
      ;; short block
   (-0.240464 0.0    -0.271646   0.285951942  0.0 -0.958243966   0.0 0.0  1 1 1)
   (-0.240464 0.165  -0.271646   0.285951942  0.0 -0.958243966   0.0 0.0  1 1 1)
   (-0.082354 0.165  -0.224464   0.285951942  0.0 -0.958243966   0.0 0.0  1 1 1)
   (-0.082354 0.0    -0.224464   0.285951942  0.0 -0.958243966   0.0 0.0  1 1 1)
   (-0.240464 0.0    -0.271646  -0.958243966  0.0 -0.285951942   0.0 0.0  1 1 1)
   (-0.240464 0.165  -0.271646  -0.958243966  0.0 -0.285951942   0.0 0.0  1 1 1)
   (-0.287646 0.165  -0.113536  -0.958243966  0.0 -0.285951942   0.0 0.0  1 1 1)
   (-0.287646 0.0    -0.113536  -0.958243966  0.0 -0.285951942   0.0 0.0  1 1 1)
   (-0.082354 0.0    -0.224464   0.958243966  0.0  0.285951942   0.0 0.0  1 1 1)
   (-0.082354 0.165  -0.224464   0.958243966  0.0  0.285951942   0.0 0.0  1 1 1)
   (-0.129536 0.165  -0.066354   0.958243966  0.0  0.285951942   0.0 0.0  1 1 1)
   (-0.129536 0.0    -0.066354   0.958243966  0.0  0.285951942   0.0 0.0  1 1 1)
   (-0.287646 0.0    -0.113536  -0.285951942  0.0  0.958243966   0.0 0.0  1 1 1)
   (-0.287646 0.165  -0.113536  -0.285951942  0.0  0.958243966   0.0 0.0  1 1 1)
   (-0.129536 0.165  -0.066354  -0.285951942  0.0  0.958243966   0.0 0.0  1 1 1)
   (-0.129536 0.0    -0.066354  -0.285951942  0.0  0.958243966   0.0 0.0  1 1 1)
   (-0.240464 0.165  -0.271646   0.0          1.0  0.0           0.0 0.0  1 1 1)
   (-0.082354 0.165  -0.224464   0.0          1.0  0.0           0.0 0.0  1 1 1)
   (-0.129536 0.165  -0.066354   0.0          1.0  0.0           0.0 0.0  1 1 1)
   (-0.287646 0.165  -0.113536   0.0          1.0  0.0           0.0 0.0  1 1 1)
      ;; tall block
   (-0.471239 0.0    -0.405353  -0.296278358  0.0 -0.955101609   0.0 0.0  1 1 1)
   (-0.471239 0.33   -0.405353  -0.296278358  0.0 -0.955101609   0.0 0.0  1 1 1)
   (-0.313647 0.33   -0.454239  -0.296278358  0.0 -0.955101609   0.0 0.0  1 1 1)
   (-0.313647 0.0    -0.454239  -0.296278358  0.0 -0.955101609   0.0 0.0  1 1 1)
   (-0.264761 0.0    -0.296647   0.955101609  0.0 -0.296278358   0.0 0.0  1 1 1)
   (-0.264761 0.33   -0.296647   0.955101609  0.0 -0.296278358   0.0 0.0  1 1 1)
   (-0.313647 0.33   -0.454239   0.955101609  0.0 -0.296278358   0.0 0.0  1 1 1)
   (-0.313647 0.0    -0.454239   0.955101609  0.0 -0.296278358   0.0 0.0  1 1 1)
   (-0.471239 0.0    -0.405353  -0.955101609  0.0  0.296278358   0.0 0.0  1 1 1)
   (-0.471239 0.33   -0.405353  -0.955101609  0.0  0.296278358   0.0 0.0  1 1 1)
   (-0.422353 0.33   -0.247761  -0.955101609  0.0  0.296278358   0.0 0.0  1 1 1)
   (-0.422353 0.0    -0.247761  -0.955101609  0.0  0.296278358   0.0 0.0  1 1 1)
   (-0.422353 0.0    -0.247761   0.296278358  0.0  0.955101609   0.0 0.0  1 1 1)
   (-0.422353 0.33   -0.247761   0.296278358  0.0  0.955101609   0.0 0.0  1 1 1)
   (-0.264761 0.33   -0.296647   0.296278358  0.0  0.955101609   0.0 0.0  1 1 1)
   (-0.264761 0.0    -0.296647   0.296278358  0.0  0.955101609   0.0 0.0  1 1 1)
   (-0.471239 0.33   -0.405353   0.0          1.0  0.0           0.0 0.0  1 1 1)
   (-0.313647 0.33   -0.454239   0.0          1.0  0.0           0.0 0.0  1 1 1)
   (-0.264761 0.33   -0.296647   0.0          1.0  0.0           0.0 0.0  1 1 1)
   (-0.422353 0.33   -0.247761   0.0          1.0  0.0           0.0 0.0  1 1 1)
))
(define indices '(
    1  3  2  1  4  3 ; floor
    5  7  6  5  8  7 ; back
    9 10 11  9 11 12 ; ceiling
   13 14 15 14 16 15 ; left wall
   17 18 19 19 18 20 ; right wall
   21 22 23 21 23 24 ; light
      ; short block
   25 26 27 25 27 28
   29 31 30 29 32 31
   33 34 35 33 35 36
   37 39 38 37 40 39
   41 43 42 41 44 43
      ; tall block
   45 46 47 45 47 48
   49 51 50 49 52 51
   53 55 54 53 56 55
   57 59 58 57 60 59
   61 63 62 61 64 63 ))

; ---------------

(define VAO (box 0))
(glGenVertexArrays 1 VAO)
(define VAO (unbox VAO))

(define VBO (box 0))
(glGenBuffers 1 VBO)
(define VBO (unbox VBO))

(define EBO (box 0)) ; index buffer
(glGenBuffers 1 EBO)
(define EBO (unbox EBO))

(glBindVertexArray VAO)
(glBindBuffer GL_ARRAY_BUFFER VBO)

(define fft-float* (fft* fft-float))
(define fft-int* (fft* fft-int))
(define vertices (apply append Vertices))
(define stride (* (sizeof fft-float) (length (car Vertices))))

(glBufferData GL_ARRAY_BUFFER
   (* (sizeof fft-float) (length vertices))
   (cons fft-float* vertices) GL_STATIC_DRAW)

; map attrib location
(glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE stride nullptr) ; 0 for "location = 0", 3 for vec3
;; (glVertexAttribPointer 1 3 GL_FLOAT GL_FALSE stride (* 8 (sizeof fft-float))) ; 0 for "location = 0", 3 for vec3
(glEnableVertexAttribArray 0)

(glBindBuffer GL_ELEMENT_ARRAY_BUFFER EBO)
(glBufferData GL_ELEMENT_ARRAY_BUFFER (* (sizeof fft-int) (length indices)) (cons fft-int* indices) GL_STATIC_DRAW)

(glBindVertexArray 0)

(define p (gl:create-program
"#version 460 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aColor;

layout (location = 0) out vec4 Color;

void main()
{
    gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);
    Color = vec4(aColor.x, aColor.y, aColor.z, 1.0);
}"

"#version 460 core
out vec4 FragColor;

layout (location = 0) in vec4 Color;

void main()
{
    FragColor = vec4(0.7, 0.6, 0.0, 1.0); //Color;
}"))


; draw loop
(gl:set-renderer (lambda ()
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glUseProgram p)
   (glBindVertexArray VAO)
   ;(glDrawElements GL_TRIANGLES (length indices) GL_UNSIGNED_INT 0)
   (glDrawElements GL_TRIANGLES 6 GL_UNSIGNED_INT 0)
   ;; (glDrawArrays GL_LINE_LOOP 0 4))) ; totally 4 points
))

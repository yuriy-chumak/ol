#!/usr/bin/ol
; for intel gpu: apt-get install beignet
; http://www.freedesktop.org/wiki/Software/Beignet/
; https://01.org/beignet
(define *path* (cons ".." *path*))
(import (OpenCL version-1-0))

(define (new-size_t) (make-bytevector (size nullptr)))
(define (new-int)    (make-bytevector '(0 0 0 0)))

(define (vector->ptr vector delta)
   (let ((wordsize (size nullptr)))
   (let loop ((int 0) (n 0) (b 0))
;     (print int "-" n "-" b ":" (refb vector 0))
      (if (< n wordsize)
         (loop (+ int (<< (refb vector (+ n delta)) b)) (+ n 1) (+ b 8))
         int))))

(define (check error)
   (case error
      (CL_SUCCESS #t) ; nothing
      (CL_DEVICE_NOT_FOUND       (runtime-error "Device not found:" error))
      (CL_DEVICE_NOT_AVAILABLE   (runtime-error "Device not available:" error))
      (CL_COMPILER_NOT_AVAILABLE (runtime-error "Compiler not available:" error))
      (CL_INVALID_VALUE          (runtime-error "Invalid value:" error))
      (CL_INVALID_DEVICE_TYPE    (runtime-error "Invalid device type:" error))
      (else (runtime-error "Unknown error:" error))))

(define (->int vector)
   (+ (<< (ref vector 0)  0)
      (<< (ref vector 1)  8)
      (<< (ref vector 2) 16)
      (<< (ref vector 3) 24)))


(define n (make-bytevector '(0 0 0 0)))
(check
   (clGetPlatformIDs 0 null n))
(define n (->int n))
(print "Number of available platforms: " n)

(define platforms (make-bytevector (repeat 0 (* n (size nullptr)))))
(check
   (clGetPlatformIDs n platforms null))
(define platform (vector->ptr platforms 0))
(print "Platform id: " platform)

(define (cl:GetPlatformInfo platform name)
   (let*((length (make-bytevector '(0 0 0 0)))
         (_ (check (clGetPlatformInfo platform name 0 null length)))
         (length (->int length))
         (info (vm:makeb type-string (repeat 0 length)))
         (_ (check (clGetPlatformInfo platform name length info null))))
      info))

(print "Platform name: "    (cl:GetPlatformInfo platform CL_PLATFORM_NAME))
(print "Platform version: " (cl:GetPlatformInfo platform CL_PLATFORM_VERSION))
(print "Platform vendor: "  (cl:GetPlatformInfo platform CL_PLATFORM_VENDOR))
(print "Platform profile: " (cl:GetPlatformInfo platform CL_PLATFORM_PROFILE))
(print "Extensions: " (cl:GetPlatformInfo platform CL_PLATFORM_EXTENSIONS) "\n")


(define devices-count (make-bytevector '(0 0 0 0)))
(check ;"clGetDeviceIDs: "
   (clGetDeviceIDs platform CL_DEVICE_TYPE_GPU 0 '() devices-count))
(define devices-count (->int devices-count))
(print "devices count: " devices-count)

(define devices (make-bytevector (repeat 0 (* devices-count (size nullptr)))))
(check
   (clGetDeviceIDs platform CL_DEVICE_TYPE_ALL devices-count devices null))
(define device (vector->ptr devices 0))
(print "First device id: " device)


(define (cl:GetDeviceInfoString device name)
   (let*((length (make-bytevector '(0 0 0 0)))
         (_ (check (clGetDeviceInfo device name 0 null length)))
         (length (->int length))
         (info (vm:makeb type-string (repeat 0 length)))
         (_ (check (clGetDeviceInfo device name length info null))))
      info))

(print "Device name: "    (cl:GetDeviceInfoString device CL_DEVICE_NAME))
(print "Device version: " (cl:GetDeviceInfoString device CL_DEVICE_VERSION))
(print "Device venrod: "  (cl:GetDeviceInfoString device CL_DEVICE_VENDOR))
(print "Device profile: " (cl:GetDeviceInfoString device CL_DEVICE_PROFILE))
(print "Device driver: "  (cl:GetDeviceInfoString device CL_DRIVER_VERSION))

;(define devices-count (->int devices-count))

(print "devices: " devices)
(define error (make-bytevector '(0 0 0 0)))
(define context (clCreateContext null 1 devices null null error))

(print "clCreateContext: error = " error ", context:" context)

(define (cl:CreateProgramWithSource context source-code)
   (clCreateProgramWithSource context 1 (list (c-string source-code)) null error))

(define program (cl:CreateProgramWithSource context "__kernel
   void simple_demo(__global int *src, __global int *dst, int factor)
   {
      int i = get_global_id(0);
      dst[i] = src[i] * factor;
   }
"))
(print "program: " program ", " error)

(define built (clBuildProgram program 1 devices "" null null))
(check built)

;(if (not (eq? CL_SUCCESS built))
;   (begin
;      (define sizeof-log 256)
;      (define log (vm:makeb type-string (repeat 0 sizeof-log)))
;      (clGetProgramBuildInfo program device CL_PROGRAM_BUILD_LOG sizeof-log log null)
;      (print "CL Compilation failed: " log)))





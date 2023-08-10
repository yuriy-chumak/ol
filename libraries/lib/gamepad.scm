(define-library (lib gamepad)
   (version 1.0)
   (license MIT/LGPL3)
   (description "gamepad support library")
(import
   (otus lisp) (otus ffi))

(export
   axis-count
   buttons-count
   read-event)

(cond-expand
   (Windows
      (begin
         #false
      ))
   (Android
      (begin
         #false

      ))
   (Linux
      (begin
         ; /usr/include/linux/joystick.h
         (define js0 (open-binary-input-file "/dev/input/js0"))
         (define ioctl ((load-dynamic-library #false) fft-int "ioctl" type-port fft-unsigned-long))

         (define (axis-count)
            (define count (box 0))
            (ioctl js0 2147576337 (cons (fft& fft-char) count)) ;JSIOCGAXES
            (unbox count))

         (define (buttons-count)
            (define count (box 0))
            (ioctl js0 2147576338 (cons (fft& fft-char) count)) ;JSIOCGBUTTONS
            (unbox count))

         (define (read-event)
            (when (port? js0)
               (define bytes (syscall 0 js0 8))
               (when (bytevector? bytes) [
                  ; 0..3 is a timestamp (is not used)
                  (bytevector->int16 bytes 4) ; value
                  (ref bytes 6) (ref bytes 7) ; event type and axis/button number
               ])))
      ))
   (Darwin
      (begin
         #false
      ))
   (else
      (begin
         (runtime-error "Unsupported platform" (syscall 63))))))

(define-library (lib portaudio)
   (version 1.0)
   (license MIT/LGPL3)
   (description "lib portaudio interface")
   (export
      Pa_GetVersion
      Pa_GetVersionText
      Pa_GetErrorText

      Pa_Initialize

      Pa_GetDeviceCount
      Pa_GetDefaultInputDevice
      Pa_GetDefaultOutputDevice

      Pa_OpenDefaultStream
         paFloat32
         paInt32
         paInt24
         paInt16
         paInt8
         paUInt8
         paCustomFormat
         paNonInterleaved
      Pa_StartStream
      Pa_StopStream
      Pa_AbortStream
      Pa_ReadStream
   )
(import
   (scheme core)
   (otus ffi))

(cond-expand
   (Linux
      (begin
         (define PA (load-dynamic-library "libportaudio.so"))))
   (else
      (runtime-error "Unsupported platform" (uname))))

(begin
   (setq int fft-int)
   (setq char* type-string)
   (setq void* type-vptr)

   (setq PaError int)
   (setq PaDeviceIndex int)

   (setq PaStream* type-vptr)
   (setq PaStream** (fft& PaStream*))
   (setq PaSampleFormat fft-unsigned-long)
   (setq PaStreamCallback* type-callable)

   (define Pa_GetVersion (PA int "Pa_GetVersion"))
   (define Pa_GetVersionText (PA char* "Pa_GetVersionText"))
   (define Pa_GetErrorText (PA char* "Pa_GetErrorText" PaError))

   (define Pa_Initialize (PA PaError "Pa_Initialize"))

   (define Pa_GetDeviceCount (PA PaDeviceIndex "Pa_GetDeviceCount"))
   (define Pa_GetDefaultInputDevice (PA PaDeviceIndex "Pa_GetDefaultInputDevice"))
   (define Pa_GetDefaultOutputDevice (PA PaDeviceIndex "Pa_GetDefaultOutputDevice"))

   (define Pa_OpenDefaultStream (PA PaError "Pa_OpenDefaultStream"
               PaStream** ; PaStream** stream
               int ; int numInputChannels
               int ; int numOutputChannels
               PaSampleFormat ; PaSampleFormat sampleFormat
               fft-double ; double sampleRate
               fft-unsigned-long ; unsigned long framesPerBuffer
               PaStreamCallback* ; PaStreamCallback *streamCallback
               void* ; void *userData
   ))
      (define paFloat32        #x00000001)
      (define paInt32          #x00000002)
      (define paInt24          #x00000004)
      (define paInt16          #x00000008)
      (define paInt8           #x00000010)
      (define paUInt8          #x00000020)
      (define paCustomFormat   #x00010000)
      (define paNonInterleaved #x80000000)

   (define Pa_StartStream (PA PaError "Pa_StartStream" PaStream*))
   (define Pa_StopStream (PA PaError "Pa_StopStream" PaStream*))
   (define Pa_AbortStream (PA PaError "Pa_AbortStream" PaStream*))

   (define Pa_ReadStream (PA PaError "Pa_ReadStream" PaStream* type-bytevector fft-unsigned-long))

))
(define-library (lib soil)
   (import (otus lisp)
           (otus ffi))
   (export
      SOIL_last_result
      SOIL_load_OGL_texture
      SOIL_load_OGL_texture_from_memory

      SOIL_load_image
      SOIL_free_image_data
      
      SOIL_LOAD_AUTO
      SOIL_LOAD_L
      SOIL_LOAD_LA
      SOIL_LOAD_RGB
      SOIL_LOAD_RGBA

      SOIL_CREATE_NEW_ID

      SOIL_FLAG_INVERT_Y
   )
(cond-expand
   (Windows
      (begin
         (define libsoil (or (load-dynamic-library "soil.dll")
                             (runtime-error "Can't load libsoil"
                                      "try to download soil.dll")))))
   (Android
      (begin
         (define libsoil (or (load-dynamic-library "libSOIL.so")
                             (runtime-error "Can't load libsoil"
                                      "try to rebuild apk")))))
   (else
      (begin
         (define libsoil (or (load-dynamic-library "libSOIL.so.1")
                             (runtime-error "Can't load libsoil"
                                      "try to 'apt install libsoil1', for example"))))))

(begin
	; The format of images that may be loaded (force_channels)
   (define SOIL_LOAD_AUTO 0) ; leaves the image in whatever format it was found
   (define SOIL_LOAD_L    1) ; forces the image to load as Luminous (greyscale)
   (define SOIL_LOAD_LA   2) ; forces the image to load as Luminous with Alpha
   (define SOIL_LOAD_RGB  3) ; forces the image to load as Red Green Blue
   (define SOIL_LOAD_RGBA 4) ; forces the image to load as Red Green Blue Alpha
   
   (define SOIL_CREATE_NEW_ID 0)
   (define SOIL_FLAG_POWER_OF_TWO 1)
   (define SOIL_FLAG_MIPMAPS 2)
   (define SOIL_FLAG_TEXTURE_REPEATS 4)
   (define SOIL_FLAG_MULTIPLY_ALPHA 8)
   (define SOIL_FLAG_INVERT_Y 16)
   (define SOIL_FLAG_COMPRESS_TO_DXT 32)
   (define SOIL_FLAG_DDS_LOAD_DIRECT 64)
   (define SOIL_FLAG_NTSC_SAFE_RGB 128)
   (define SOIL_FLAG_CoCg_Y 256)
   (define SOIL_FLAG_TEXTURE_RECTANGLE 51)

   ;SOIL_SAVE_TYPE_TGA = 0,
   ;SOIL_SAVE_TYPE_BMP = 1,
   ;SOIL_SAVE_TYPE_DDS = 2
   
   ;SOIL_DDS_CUBEMAP_FACE_ORDER "EWUDNS"
   
   ;SOIL_HDR_RGBE = 0,
   ;SOIL_HDR_RGBdivA = 1,
   ;SOIL_HDR_RGBdivA2 = 2

   (define SOIL_last_result (libsoil type-string "SOIL_last_result"))
   
   (define SOIL_load_OGL_texture (libsoil fft-unsigned-int "SOIL_load_OGL_texture" type-string fft-int fft-unsigned-int fft-unsigned-int))
   (define SOIL_load_OGL_texture_from_memory (libsoil fft-unsigned-int "SOIL_load_OGL_texture_from_memory" type-bytevector fft-int fft-int fft-unsigned-int fft-unsigned-int))
   (define SOIL_load_OGL_cubemap (libsoil fft-unsigned-int "SOIL_load_OGL_cubemap" type-string type-string type-string type-string type-string type-string fft-int fft-unsigned-int fft-unsigned-int))
   ;SOIL_load_OGL_cubemap_from_memory
   (define SOIL_load_OGL_single_cubemap (libsoil fft-unsigned-int "SOIL_load_OGL_single_cubemap" type-string type-string fft-int fft-unsigned-int fft-unsigned-int))
   ;SOIL_load_OGL_single_cubemap_from_memory
   ;SOIL_load_OGL_HDR_texture

   (define SOIL_load_image (libsoil type-vptr "SOIL_load_image" type-string fft-int& fft-int& fft-int& fft-int))
   ;SOIL_load_image_from_memory
   (define SOIL_free_image_data (libsoil fft-void "SOIL_free_image_data" type-vptr))
   
))

(define-library (lib soil)
   (import (otus lisp)
           (otus ffi))
   (export
      SOIL_last_result
      SOIL_load_OGL_texture
      SOIL_load_OGL_texture_from_memory

      SOIL_load_OGL_cubemap
      SOIL_load_OGL_cubemap_from_memory
      SOIL_load_OGL_single_cubemap
      SOIL_load_OGL_single_cubemap_from_memory

      SOIL_load_image
      SOIL_load_image_from_memory
      SOIL_free_image_data

      SOIL_create_OGL_texture
      SOIL_save_image
      
      SOIL_LOAD_AUTO
      SOIL_LOAD_L
      SOIL_LOAD_LA
      SOIL_LOAD_RGB
      SOIL_LOAD_RGBA

      SOIL_CREATE_NEW_ID
      
      SOIL_DDS_CUBEMAP_FACE_ORDER

      SOIL_FLAG_INVERT_Y

      SOIL_SAVE_TYPE_TGA
      SOIL_SAVE_TYPE_BMP
      SOIL_SAVE_TYPE_DDS

      SOIL_save_screenshot

      SOIL_FLAG_MIPMAPS
   )

(cond-expand
   (Windows
      (begin
         (define libsoil (or (load-dynamic-library "soil.dll")
                             (runtime-error "Can't load libsoil"
                                      "try to install 'soil.dll' from https://github.com/yuriy-chumak/libol-soil/releases/")))))
   (Android
      (begin
         (define libsoil (or (load-dynamic-library "libSOIL.so")
                             (load-dynamic-library "libSOIL.so.1")
                             (runtime-error "Can't load libsoil"
                                      "try to rebuild apk")))))
   (else
      (begin
         (define libsoil (or (load-dynamic-library "libSOIL.so.1")
                             (runtime-error "Can't load libsoil"
                                      "try to install 'libsoil1' package, or\n"
                               "       check the libsoil homepage at https://github.com/yuriy-chumak/libol-soil\n"
                               "       if there is no package for your OS"))))))

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

   (define SOIL_SAVE_TYPE_TGA 0)
   (define SOIL_SAVE_TYPE_BMP 1)
   (define SOIL_SAVE_TYPE_DDS 2)
   
   (define SOIL_DDS_CUBEMAP_FACE_ORDER "EWUDNS")
   
   ;SOIL_HDR_RGBE = 0,
   ;SOIL_HDR_RGBdivA = 1,
   ;SOIL_HDR_RGBdivA2 = 2

   (define int fft-int)
   (define int& fft-int&)

   (define SOIL_last_result (libsoil type-string "SOIL_last_result"))
   
   (define SOIL_load_OGL_texture (libsoil fft-unsigned-int "SOIL_load_OGL_texture" type-string fft-int fft-unsigned-int fft-unsigned-int))
   (define SOIL_load_OGL_texture_from_memory (libsoil fft-unsigned-int "SOIL_load_OGL_texture_from_memory" type-vptr fft-int fft-int fft-unsigned-int fft-unsigned-int))

   (define SOIL_load_OGL_cubemap (libsoil fft-unsigned-int "SOIL_load_OGL_cubemap"
      type-string type-string type-string type-string type-string type-string
      fft-int fft-unsigned-int fft-unsigned-int))
   (define SOIL_load_OGL_cubemap_from_memory (libsoil fft-unsigned-int "SOIL_load_OGL_cubemap_from_memory"
      type-vptr fft-int ; x positive
      type-vptr fft-int ; x negative
      type-vptr fft-int ; y positive
      type-vptr fft-int ; y negative
      type-vptr fft-int ; z positive
      type-vptr fft-int ; z negative
      fft-int fft-unsigned-int fft-unsigned-int))

   (define SOIL_load_OGL_single_cubemap (libsoil fft-unsigned-int "SOIL_load_OGL_single_cubemap"
      type-string type-string fft-int fft-unsigned-int fft-unsigned-int))
   (define SOIL_load_OGL_single_cubemap_from_memory (libsoil fft-unsigned-int "SOIL_load_OGL_single_cubemap_from_memory"
      type-vptr fft-int
      type-string
      fft-int fft-unsigned-int fft-unsigned-int))

   ;SOIL_load_OGL_single_cubemap_from_memory
   ;SOIL_load_OGL_HDR_texture

   (define SOIL_load_image (libsoil type-vptr "SOIL_load_image" type-string int& int& int& int))
   (define SOIL_load_image_from_memory (libsoil type-vptr "SOIL_load_image_from_memory" type-vptr int int& int& int& int))
   (define SOIL_free_image_data (libsoil fft-void "SOIL_free_image_data" type-vptr))
   
   (define SOIL_create_OGL_texture (libsoil fft-int "SOIL_create_OGL_texture" fft-any fft-int fft-int fft-int fft-int fft-int))
   (define SOIL_save_image (libsoil fft-int "SOIL_save_image" type-string fft-int fft-int fft-int fft-int fft-any))

   (define SOIL_save_screenshot (libsoil fft-int "SOIL_save_screenshot" type-string fft-int fft-int fft-int fft-int fft-int))
))

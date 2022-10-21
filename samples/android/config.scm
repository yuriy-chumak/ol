(define-library (config)
   (import (scheme core))
   (export
   ;; vr screen size in meters
      vr-screen-size
   ;; lens separation distance
      lens-separation-distance

      lens-distortion-values
      chroma-ab-correction
)
(begin
   ; Oculus Go Screen:
   ; 2560x1440 (538ppi)
   ;     4,758364312 inch x 2,676579926 inch
   ;     0,120862454 cm   x 0,06798513
   ; 1 inch = 0.0254 meters (or divide by 39.37)
   ; http://www.sitesinvr.com/viewer/settings.htm
   ;
   ; SetLensPositions - iad=0.0635, left offset=0.00151,0, right offset=-0.00151,0
   ; SetLensPositions - iad=0.063, left offset=0.000250001,0, right offset=-0.000250001,0
   ; lensSeparationMeters = 0.063500
   ; horizontalOffsetMeters = 0.000000
   ; displayWidthMeters = 0.120960
   ; displayHeightMeters = 0.068040
   ; displayWidthPixels = 2560
   ; displayHeightPixels = 1440
   ; eyeTextureWidthPixels = 1024

   (define vr-screen-size [#i0.12086245 #i0.06798513])
   (define lens-separation-distance #i0.0604) ; почти точно половина ширины

   (define lens-distortion-values [#i1.0 #i0.11 #i0.26 0])
   (define chroma-ab-correction [#i0.996 #i-0.004 #i1.014 0])
))
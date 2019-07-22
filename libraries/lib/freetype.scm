(define-library (lib freetype)
(export
   make-FT_Library
   make-FT_Face

   FT_Init_FreeType
   FT_New_Face
   FT_Set_Char_Size
   FT_Set_Pixel_Sizes

   FT_Get_Char_Index

   FT_Load_Glyph
      FT_LOAD_DEFAULT
   FT_Load_Char
      FT_LOAD_RENDER

   FT_Render_Glyph
      FT_RENDER_MODE_NORMAL

   FT_Done_Face
   FT_Done_FreeType

   face->glyph glyph->bitmap
   )

(import
   (otus lisp)
   (otus ffi))
(begin

   (define (make-FT_Library) (make-vptr))
   (define (make-FT_Face) (make-vptr))

   (setq FT_Error fft-int)
   (setq FT_Library fft-void*) (setq FT_Library* (fft* FT_Library))
   (setq FT_Face fft-void*)    (setq FT_Face* (fft* FT_Face))

   (setq FT_Long fft-long) ;?
   (setq FT_UInt fft-unsigned-int) ;?
   (setq FT_ULong fft-unsigned-long)
   (setq FT_Int32 fft-int32)
   (setq FT_F26Dot6 fft-signed-long)
   (setq FT_GlyphSlot fft-void*)
   (setq FT_Render_Mode fft-int)
)
(cond-expand
   (Android
      (begin
         (setq freetype (load-dynamic-library #false)))) ; "libfreetype.so"
   (else
      (begin
         (setq freetype (load-dynamic-library "libfreetype.so.6")))))

(begin

   (define FT_Init_FreeType (freetype FT_Error "FT_Init_FreeType" FT_Library*))
   (define FT_Done_FreeType (freetype FT_Error "FT_Done_FreeType" FT_Library))

   (define FT_New_Face (freetype FT_Error "FT_New_Face" FT_Library type-string FT_Long FT_Face*))
   (define FT_Done_Face (freetype FT_Error "FT_Done_Face" FT_Face))

   (define FT_Set_Char_Size (freetype FT_Error "FT_Set_Char_Size" FT_Face FT_F26Dot6 FT_F26Dot6 FT_UInt FT_UInt))
   (define FT_Set_Pixel_Sizes (freetype FT_Error "FT_Set_Pixel_Sizes" FT_Face FT_UInt FT_UInt))

   (define FT_Get_Char_Index (freetype FT_UInt "FT_Get_Char_Index" FT_Face FT_ULong))

   (define FT_Load_Glyph (freetype FT_Error "FT_Load_Glyph" FT_Face FT_UInt FT_Int32))
      (define FT_LOAD_DEFAULT 0)
   (define FT_Load_Char (freetype FT_Error "FT_Load_Char" FT_Face FT_ULong FT_Int32))
      (define FT_LOAD_RENDER (<< 1 2))

   (define FT_Render_Glyph (freetype FT_Error "FT_Render_Glyph" FT_GlyphSlot FT_Render_Mode))
      (define FT_RENDER_MODE_NORMAL 0)

   ;; (setq FT_String* fft-void*)
   ;; (setq FT_Int fft-int)
   ;; (setq FT_Bitmap_Size* fft-void*)
   ;; (setq FT_CharMap* fft-void*)
   ;; (setq FT_CharMap fft-void*)
   ;; (setq FT_Generic_Finalizer fft-void*)
   ;; (setq FT_Generic (list fft-void* FT_Generic_Finalizer))
   ;; (setq FT_Pos fft-signed-long)
   ;; (setq FT_BBox (list FT_Pos FT_Pos FT_Pos FT_Pos))
   ;; (setq FT_UShort fft-unsigned-short)
   ;; (setq FT_Short fft-short)
   ;; (setq FT_GlyphSlot fft-void*)
   ;; (setq FT_Size fft-void*)

   ;; (setq size-of-FT_Face (sizeof (list
   ;;       FT_Long ; num_faces
   ;;       FT_Long ; face_index

   ;;       FT_Long ; face_flags
   ;;       FT_Long ; style_flags

   ;;       FT_Long ; num_glyphs;

   ;;       FT_String* ; family_name;
   ;;       FT_String* ; style_name;

   ;;       FT_Int           ; num_fixed_sizes;
   ;;       FT_Bitmap_Size*  ; available_sizes;

   ;;       FT_Int           ; num_charmaps;
   ;;       FT_CharMap*      ; charmaps;

   ;;       FT_Generic       ; generic;

   ;;       ;; /*# The following member variables (down to `underline_thickness') */
   ;;       ;; /*# are only relevant to scalable outlines; cf. @FT_Bitmap_Size    */
   ;;       ;; /*# for bitmap fonts.                                              */
   ;;       FT_BBox          ; bbox;

   ;;       FT_UShort        ; units_per_EM;
   ;;       FT_Short         ; ascender;
   ;;       FT_Short         ; descender;
   ;;       FT_Short         ; height;

   ;;       FT_Short         ; max_advance_width;
   ;;       FT_Short         ; max_advance_height;

   ;;       FT_Short         ; underline_position;
   ;;       FT_Short         ; underline_thickness;

   ;;       FT_GlyphSlot     ; glyph;
   ;;       FT_Size          ; size;
   ;;       FT_CharMap))) ; charmap
   ;; (define offset-of-FT_Face-glyph (sizeof (list
   ;;       FT_Long FT_Long FT_Long FT_Long
   ;;       FT_Long
   ;;       FT_String* FT_String*
   ;;       FT_Int
   ;;       FT_Bitmap_Size*
   ;;       FT_Int
   ;;       FT_CharMap*
   ;;       FT_Generic
   ;;       FT_BBox
   ;;       FT_UShort FT_Short FT_Short FT_Short FT_Short FT_Short
   ;;       FT_Short FT_Short)))

   ;; (setq FT_Glyph_Metrics (list FT_Pos FT_Pos  FT_Pos FT_Pos FT_Pos  FT_Pos FT_Pos FT_Pos))
   ;; (setq FT_Fixed fft-signed-long)
   ;; (setq FT_Vector (list FT_Pos FT_Pos))
   ;; (setq FT_Glyph_Format fft-enum)
   ;; (setq FT_Bitmap (list
   ;;    fft-unsigned-int ; rows
   ;;    fft-unsigned-int ; width
   ;;    fft-int ; pitch
   ;;    fft-void* ; buffer
   ;;    fft-unsigned-short ; num_grays
   ;;    fft-unsigned-char ; pixel_mode
   ;;    fft-unsigned-char ; palette_mode
   ;;    fft-void*)) ; palette

   ;; (define size-of-FT_GlyphSlot (sizeof (list
   ;;    FT_Library FT_Face FT_GlyphSlot FT_UInt FT_Generic
   ;;    FT_Glyph_Metrics FT_Fixed FT_Fixed FT_Vector
   ;;    FT_Glyph_Format FT_Bitmap FT_Int FT_Int
   ;;    ;...
   ;; )))

)
(cond-expand
   (Android
      (begin
         (define (face->glyph face)
            ; sizeof(FT_FaceRec) = 132
            ; offsetof(FT_FaceRec, glyph) = 84
            (extract-void* (vptr->bytevector face 132) 84))

         (define (glyph->bitmap glyph*)
            ; sizeof(FT_GlyphSlotRec) = 160
            ; offsetof(FT_GlyphSlotRec, bitmap) = 76
            ; offsetof(FT_GlyphSlotRec, bitmap_left) = 100
            ; offsetof(FT_GlyphSlotRec, bitmap_top) = 104

            ; sizeof(FT_Bitmap) = 24
            ; offsetof(FT_Bitmap, rows) = 0
            ; offsetof(FT_Bitmap, width) = 4
            ; offsetof(FT_Bitmap, pitch) = 8
            ; offsetof(FT_Bitmap, buffer) = 12
            (let ((bitmap (make-bytevector (drop (bytevector->list (vptr->bytevector glyph* 160)) 76))))
               (let ((height (extract-number bitmap 0 4))
                     (width (extract-number bitmap 4 4))
                     (pitch (extract-number bitmap 8 4))
                     (buffer* (extract-void* bitmap 12))
                     (left (extract-number bitmap 24 4)) ; 100-76
                     (top (extract-number bitmap 28 4))) ; 104-76
                  [left width top height buffer*])))
      ))
   (else
      (begin
         (define (face->glyph face)
            (extract-void* (vptr->bytevector face 248) 152)) ; 132 and 84 for x86

         (define (glyph->bitmap glyph*)
            (let ((bitmap (make-bytevector (drop (bytevector->list (vptr->bytevector glyph* 304)) 152))))
               (let ((height (extract-number bitmap 0 4)) ; 80 for x86
                     (width (extract-number bitmap 4 4))
                     (pitch (extract-number bitmap 8 4))
                     (buffer* (extract-void* bitmap 16))
                     (left (extract-number bitmap 40 4))
                     (top (extract-number bitmap 44 4)))
                  [left width top height buffer*]))))))
)

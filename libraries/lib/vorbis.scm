(define-library (lib vorbis)
   (import
      (scheme core)
      (otus ffi))
   (export
      make-OggVorbis_File

      ov_fopen ov_clear
      ov_info ov_bitrate_instant
      ov_read
   )

(begin
   (setq malloc ((load-dynamic-library #f) fft-void* "malloc" fft-long))

   (define (make-OggVorbis_File) (malloc 944));(make-bytevector 944)) ; 704 for 23-bit, 944 for x64
   (setq OggVorbis_File* type-vptr)

   (setq libvorbis (load-dynamic-library "libvorbis.so"))
   (setq libvorbisfile (load-dynamic-library "libvorbisfile.so"))

   (define ov_clear (libvorbisfile fft-int "ov_clear" OggVorbis_File*))
   (define ov_fopen (libvorbisfile fft-int "ov_fopen" type-string OggVorbis_File*))
   (define ov_info (libvorbisfile type-vptr "ov_info" OggVorbis_File* fft-int))
   (define ov_bitrate_instant (libvorbisfile fft-long "ov_bitrate_instant" OggVorbis_File*))
   (define ov_read (libvorbisfile fft-long "ov_read" OggVorbis_File* type-vptr fft-int fft-int fft-int fft-int fft-int&))
))
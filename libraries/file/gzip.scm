(define-library (file gzip)
   ; todo: add function toi encode json into stream
   (export
      gzip-parser)
   (import
      (scheme base)
      (file parser) (owl ff) (owl math) (owl string)
      (otus inflate))
(begin

   (define gzip-parser
      (let-parse* (
            (ID1 byte)
            (ID2 byte)
            (verify (and (eq? ID1 #x1F) (eq? ID2 #x8B)) `not-a-gzip)

            (CM byte)
            (verify (eq? CM 8) `not-a-deflate)
            (FLG byte)
            (MTIME (times 4 byte))
            (XFL byte)
            (OS byte)

            ; FLG (FLaGs)
            ; FTEXT
            ; FHCRC
            ; FEXTRA
            ; FNAME
            ; FCOMMENT
            (FNAME (if (zero? (band FLG #b1000))
               (epsilon #f)
               (let-parse* (
                     (filename (greedy* (byte-if (lambda (x) (less? 0 x)))))
                     (zt (imm 0)))
                  (bytes->string filename))))

            ; header done
            ; return stream
            (data (lambda (l r p ok)
                     (ok '() r p r)))
            )
         {
            'FLG FLG
            'FNAME FNAME
            'OS (case OS
                  (0 "FAT filesystem (MS-DOS, OS/2, NT/Win32)")
                  ;; (1 "Amiga")
                  ;; (2 "VMS (or OpenVMS)")
                  ;; (3 "Unix")
                  ;; (4 "VM/CMS")
                  ;; (5 "Atari TOS")
                  ;; (6 "")
                  ;; (6 "HPFS filesystem (OS/2, NT)")
                  ;; (7 "Macintosh")
                  ;; (8 "Z-System")
                  ;; (9 "CP/M")
                  ;; (10 "TOPS-20")
                  ;; (11 "NTFS filesystem (NT)")
                  ;; (12 "QDOS")
                  ;; (13 "Acorn RISCOS")
                  (255 "unknown"))
            'stream (inflate data)
         }))

))

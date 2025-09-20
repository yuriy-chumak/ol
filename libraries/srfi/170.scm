(define-library (srfi 170)
; https://srfi.schemers.org/srfi-170/srfi-170.html
(export
   srfi-170
   ; 3.1  Error handling
   posix-error?
   posix-error-name
   posix-error-message
   ; 3.3  File system
   create-directory
   rename-file
   delete-directory
   set-file-owner
   file-info file-info?
      ; file-infos:
      file-info:device
      file-info:inode
      file-info:mode
      file-info:nlinks
      file-info:uid
      file-info:gid
      file-info:rdev
      file-info:size
      file-info:blksize
      file-info:blocks
      file-info:atime
      file-info:mtime
      file-info:ctime
      ; file-info predicates:
      file-info-directory?
      file-info-fifo?
      file-info-symlink?
      file-info-regular?
      file-info-socket?
      file-info-device?
   directory-files

   open-directory
   read-directory
   close-directory

   create-temp-file
)

(import
   (scheme core)
   (owl ff) (owl string)
   (lib c!))

(begin
   (setq srfi-170 #true)

   ; 3.1  Error handling
   (define (posix-error? obj)
      (and
         (eq? (type obj) type-value+)
         (less? 0 obj)))

   (setq posix-error-names {
       1 'EPERM    2 'ENOENT   3 'ESRCH    4 'EINTR
       5 'EIO      6 'ENXIO    7 'E2BIG    8 'ENOEXEC
       9 'EBADF   10 'ECHILD  11 'EAGAIN  12 'ENOMEM
      13 'EACCES  14 'EFAULT  16 'EBUSY   17 'EEXIST
      18 'EXDEV   19 'ENODEV  20 'ENOTDIR 21 'EISDIR
      22 'EINVAL  23 'ENFILE  24 'EMFILE  25 'ENOTTY
      27 'EFBIG   28 'ENOSPC  29 'ESPIPE  30 'EROFS
      31 'EMLINK  32 'EPIPE   33 'EDOM    34 'ERANGE
      36 'EDEADLK 38 'ENAMETOOLONG  39 'ENOLCK
      40 'ENOSYS  41 'ENOTEMPTY     42 'EILSEQ
      80 'STRUNCATE })
   ; todo: add platform-specific codes

   (define (posix-error-name posix-error)
      (posix-error-names posix-error posix-error))

   (define (posix-error-message posix-error)
      (if (posix-error? posix-error)
         (strerror posix-error)))

   ; 3.2  I/O
   ;; TBD.

   ; 3.3  File system
   ;; (create-directory fname [permission-bits])
   (define create-directory mkdir)
   ;; (create-fifo fname [permission-bits])  → undefined   POSIX mkfifo()
   ;; (create-hard-link old-fname new-fname)  → undefined   POSIX link()
   ;; (create-symlink old-fname new-fname)  → undefined   POSIX symlink()

   ;; (read-symlink fname)  → string   POSIX readlink()
   ;; (rename-file old-fname new-fname)  → undefined   POSIX rename()
   (define rename-file rename)

   ;; (delete-directory fname)  → undefined   POSIX rmdir()
   (define delete-directory rmdir)
   ;; (set-file-owner fname uid gid)  → undefined   POSIX chown()
   (define set-file-owner chown)
   ;; (set-file-times fname [access-time-object modify-time-object])  → undefined   POSIX utimensat()
   ;; (truncate-file fname/port len)  → undefined   POSIX truncate()

   ;; (file-info fname/port follow?)  → file-info-record   POSIX stat()
   (define (file-info port/file)
      (stat (if (string? port/file) (c-string port/file) port/file)))
   (define (file-info? obj)
      (and (vector? obj)
           (eq? (size obj) 13)))
   ; file-infos
   (define (file-info:device file-info)
      (if (file-info? file-info) (ref file-info 1)))
   (define (file-info:inode file-info)
      (if (file-info? file-info) (ref file-info 2)))
   (define (file-info:mode file-info)
      (if (file-info? file-info) (ref file-info 3)))
   (define (file-info:nlinks file-info)
      (if (file-info? file-info) (ref file-info 4)))
   (define (file-info:uid file-info)
      (if (file-info? file-info) (ref file-info 5)))
   (define (file-info:gid file-info)
      (if (file-info? file-info) (ref file-info 6)))
   (define (file-info:rdev file-info)
      (if (file-info? file-info) (ref file-info 7)))
   (define (file-info:size file-info)
      (if (file-info? file-info) (ref file-info 8)))
   (define (file-info:blksize file-info)
      (if (file-info? file-info) (ref file-info 9)))
   (define (file-info:blocks file-info)
      (if (file-info? file-info) (ref file-info 10)))
   (define (file-info:atime file-info)
      (if (file-info? file-info) (ref file-info 11)))
   (define (file-info:mtime file-info)
      (if (file-info? file-info) (ref file-info 12)))
   (define (file-info:ctime file-info)
      (if (file-info? file-info) (ref file-info 13)))
   ; file-info predicates
   (define (file-info-directory? file-info)
      (if (file-info? file-info)
         (eq? (vm:and (ref file-info 3) #o040000) #o040000)))
   (define (file-info-fifo? file-info)
      (if (file-info? file-info)
         (eq? (vm:and (ref file-info 3) #o010000) #o010000)))
   (define (file-info-symlink? file-info)
      (if (file-info? file-info)
         (eq? (vm:and (ref file-info 3) #o120000) #o120000)))
   (define (file-info-regular? file-info)
      (if (file-info? file-info)
         (eq? (vm:and (ref file-info 3) #o100000) #o100000)))
   (define (file-info-socket? file-info)
      (if (file-info? file-info)
         (eq? (vm:and (ref file-info 3) #o140000) #o140000)))
   (define (file-info-device? file-info)
      (if (file-info? file-info)
         (eq? (vm:and (ref file-info 3) #o060000) #o060000)))

   ;; (set-file-mode fname mode-bits)  → undefined   POSIX chmod()
   ;; (directory-files dir [dotfiles?])  → string list  
   (define except-dotfiles-selector (make-selector (lambda (filename)
      (if (eq? (ref filename 0) #\.) 0 1))))

   (define directory-files (case-lambda
      ((dir) (scandir dir except-dotfiles-selector alphasort))
      ((dir dotfiles?) (scandir dir (if dotfiles? #f except-dotfiles-selector) alphasort)) ))

   ;; (make-directory-files-generator dir [dotfiles?])  → generator  

   (define (open-directory dir dotfiles?)
      (define ptr (opendir dir))
      (if ptr
         (cons ptr dotfiles?)))
   (define open-directory (case-lambda
      ((dir) (open-directory dir #false))
      ((dir dotfiles?) (open-directory dir dotfiles?)) ))

   (define (read-directory directory-object)
      (when directory-object
         (let loop ()
            (define filename (readdir (ref directory-object 1)))
            (when filename
               (if (or (ref directory-object 2)
                       (not (eq? (ref filename 0) #\.)))
                  filename
                  (loop))))))

   (define (close-directory directory-object)
      (closedir (ref directory-object 1)))
         
   ;; (close-directory directory-object)  → undefined   POSIX closedir()

   ;; (real-path path)  → string   POSIX realpath()
   ;; (file-space path-or-port)  → exact integer   POSIX statvfs(), fstatvfs()

   (define tempfile-prefix "/tmp/ol2_")
   (define (create-temp-file prefix)
      (mktemp (string-append prefix "XXXXXX")))

   (define create-temp-file (case-lambda
      ((prefix) (create-temp-file prefix))
      (() (create-temp-file tempfile-prefix))))

   ;; (call-with-temporary-filename maker [prefix])  → object+  

   ;; (current-directory)  → string   POSIX getcwd()
   ;; (set-current-directory! new-directory)  → unspecified   POSIX chdir()

;; 3.10  Time
   ;; (posix-time)→time-object        POSIX clock_gettime()
   ;; (monotonic-time)→time-object        POSIX clock_gettime()

;; 3.11  Environment variables
   ;; (get-environment-variables) * already in the r7rs
   ;; (get-environment-variable name)
   ;; (set-environment-variable! name value)  → undefined   POSIX setenv
   ;; (delete-environment-variable! name)  → undefined   POSIX unsetenv
;; 3.12  Terminal device control
   ;; (terminal? port)  → boolean   POSIX isatty()
))

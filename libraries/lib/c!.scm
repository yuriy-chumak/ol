(define-library (lib c!)
   (version 1.0)
   (description "
      Experimental libc library interface.")

   (export
      errno strerror
      stat
      ;; 14.1 Working Directory
      getcwd chdir
      ;; 14.2 Accessing Directories
      opendir readdir closedir
      rewinddir telldir seekdir
      scandir alphasort versionsort
      make-selector make-comparer
      file? folder?
      file-selector folder-selector
      ;; 14.6 Deleting Files
      unlink rmdir remove
      ;; 14.7 Renaming Files
      rename
      ;; 14.8 Creating Directories
      mkdir
      ;; 14.9 File Attributes
      chown chmod
      truncate
      ;; 14.11 Temporary Files
      mktemp mkstemp
      ;; some olvm syscalls
      uname
      isatty
      getenv setenv)

   (import
      (scheme core)
      (otus ffi)
      (scheme bytevector)

      (owl string)
      (owl io)
      (owl math))

(cond-expand
   (Linux
      (begin
         (setq SO (or
            (load-dynamic-library "libc.so")
            (load-dynamic-library "libc.so.6")))

         ; todo: calculate at runtime
         (setq sizeof-dirent (if (eq? (size nullptr) 4) 268 280))
         (setq dirent-d_name (if (eq? (size nullptr) 4)  11  19))
      ))
   (Windows
      (begin
         (setq SO (or                        ; vcrunXXXX
            (load-dynamic-library "msvcr71.dll")  ; 2003
            (load-dynamic-library "msvcr80.dll")  ; 2005
            (load-dynamic-library "msvcr90.dll")  ; 2008
            (load-dynamic-library "msvcr100.dll") ; 2010
            (load-dynamic-library "msvcr110.dll") ; 2012
            (load-dynamic-library "msvcr120.dll") ; 2013
            (load-dynamic-library "ucrtbase.dll") ; 2015, 2017, 2019, ...
            (runtime-error "No Visual C++ Redistributables are found. Please, install at least one."))) ))
   (Darwin
      (begin
         (setq SO
            (load-dynamic-library "libSystem.dylib"))

         (setq sizeof-dirent 1048)
         (setq dirent-d_name   21)
      ))
   (else
      (begin
         (runtime-error "Unsupported platform" *uname*) )))

; types
(begin
   (setq DIR* type-vptr)
   (setq dirent* type-vptr)
   (setq dirent->string (lambda (dirent)
      (vptr->string (bytevector-copy (vptr->bytevector dirent sizeof-dirent) dirent-d_name))))
)

; 14. File System Interface
(cond-expand
   ((or Linux Darwin)
      (begin
         (setq :strerror (SO type-string "strerror" fft-int))
         ; Working Directory
         (setq :getcwd (SO type-string "getcwd" type-string fft-int))
         (setq :chdir (SO fft-int "chdir" type-string)) ; todo: fchdir
         ; Accessing Directories
         (setq :opendir (SO DIR* "opendir" type-string)) ; todo: fdopendir, dirfd
         (setq :readdir (SO dirent* "readdir" DIR*))
         (setq :closedir (SO fft-int "closedir" DIR*))
         (setq :rewinddir (SO fft-void "rewinddir" DIR*))
         (setq :telldir (SO fft-long "telldir" DIR*))
         (setq :seekdir (SO fft-void "seekdir" DIR* fft-long))
         (setq :scandir (SO fft-int "scandir" type-string (fft& type-vptr) type-callable type-callable))
         ; Deleting Files
         (setq :unlink (SO fft-int "unlink" type-string))
         (setq :rmdir (SO fft-int "rmdir" type-string))
         (setq :remove (SO fft-int "remove" type-string))
         ; Renaming Files
         (setq :rename (SO fft-int "rename" type-string type-string))
         ; 
         (setq :mkdir (SO fft-int "mkdir" type-string fft-unsigned-int))
         (setq :chown (SO fft-int "chown" type-string fft-unsigned-int fft-unsigned-int))
         (setq :fchown (SO fft-int "fchown" type-port fft-unsigned-int fft-unsigned-int))
         (setq :chmod (SO fft-int "chmod" type-string fft-unsigned-int))
         (setq :fchmod (SO fft-int "fchmod" type-port fft-unsigned-int))
         ;; (setq :access (SO fft-int "access" type-string fft-int))
         ;; (setq :truncate (SO fft-int "truncate" type-string fft-signed-long))
         ; Temporary Files
         (setq :mktemp (SO type-string "mktemp" type-string))
         (setq :mkstemp (SO type-port "mkstemp" type-string))


         ;; (setq :mkdir (SO ))
         (setq free (SO fft-void "free" type-vptr))
      ))
   (Windows
      (begin
         (setq :getcwd (SO type-string-wide "_wgetcwd" type-string-wide fft-int))
         (setq :chdir (SO fft-int "_wchdir" type-string-wide))
         (runtime-error
            "Windows platform supported in progress" #n)
      )) )

(begin
   (define (errno) (syscall 60))
   (define (uname) (syscall 63))

   (define strerror :strerror)
   (define stat (case-lambda
      ((filename) (syscall 4 (c-string filename)))
      ((filename follow) (syscall 4 (c-string filename) follow))))

   ; -- libc --------------------------------------
   (define (getcwd)
      (let loop ((n 128))
         (define cwd (:getcwd (make-string n 0) n))
         (if cwd cwd (loop (+ n 64)))))

   (define chdir :chdir)
   (define opendir :opendir)
   (define (readdir dir)
      (define ep (:readdir dir))
      (when ep
         (dirent->string ep)))
   (define closedir :closedir)
   (define rewinddir :rewinddir)
   (define telldir :telldir)
   (define seekdir :seekdir)

   (define alphasort (vm:cast (dlsym (ref SO 2) "alphasort") type-callable))
   (define versionsort (vm:cast (dlsym (ref SO 2) "versionsort") type-callable))

   (define (file? filename)
      (let ((v (stat filename)))
         (when v
            (zero? (band (ref v 3) #o0040000)))))
   (define (folder? filename)
      (let ((v (stat filename)))
         (when v
            (not (zero? (band (ref v 3) #o0040000))))))


   (define (make-selector thunk)
      (make-callback (vm:pin (cons
         (cons fft-int (list
         ;  struct dirent *
            type-vptr))
         (lambda (a)
            (thunk (dirent->string a)))))))

   (define (make-comparer thunk)
      (make-callback (vm:pin (cons
         (cons fft-int (list
         ;  struct dirent **  struct dirent **
            type-vptr         type-vptr))
         (lambda (a b)
            (let*((pa (bytevector->void* (vptr->bytevector a (size nullptr)) 0))
                  (pb (bytevector->void* (vptr->bytevector b (size nullptr)) 0)))
               (thunk (dirent->string pa) (dirent->string pb))))))))

   (define folder-selector (make-selector (lambda (filename)
      (if (folder? filename) 1 0))))

   (define file-selector (make-selector (lambda (filename)
      (if (file? filename) 1 0))))

   (define (scandir filename selector comparer)
      (define ptr (vm:cast 0 type-vptr))
      (define n (:scandir filename ptr selector comparer))
      (define ptrs (vptr->bytevector ptr (* n (size ptr))))
      (let loop ((n n) (out #null))
         (if (eq? n 0)
            out
         else
            (define dirent (bytevector->void* ptrs (* (-- n) (size ptr))))
            (define string (dirent->string dirent))
            (free dirent)
            (loop (-- n) (cons string out)))))

   (define unlink :unlink)
   (define rmdir :rmdir)
   (define remove :remove)
   (define rename :rename)

   (define mkdir (case-lambda
      ((filename) (:mkdir filename #o755)) ; drwxr-xr-x
      ((filename mode) (:mkdir filename mode))))

   (define (chown file owner group)
      (cond
         ((port? file)  (:fchown file owner group))
         ((string? file) (:chown file owner group))))

   (define (chmod file mode)
      (cond
         ((port? file)  (:fchmod file mode))
         ((string? file) (:chmod file mode))))

   (define mktemp :mktemp)
   (define mkstemp :mkstemp)

   ;; (define access :access)

   ;; 14.8 Creating Directories

   ; -- olvm --------------------
   (define (isatty fd)
      (syscall 16 fd 19))

   (define (getenv str)
      (syscall 1016 str))

   (define setenv (case-lambda
      ((env value)
         (syscall 1017 env value))
      ((env value overwrite)
         (syscall 1017 env value overwrite))))
))

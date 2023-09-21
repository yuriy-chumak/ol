(define-library (olvm features)
   (export
      vm:feature?

      OLVM
      OLVM_NOMAIN
      OLVM_FFI
      OLVM_CALLABLES
      OLVM_INEXACTS
      OLVM_BUILTIN_FMATH
      SYSCALL_SYSINFO
      SYSCALL_MEMFD
      SYSCALL_GETRLIMIT
      SYSCALL_GETRUSAGE
      HAS_UNSAFES
      HAVE_DLOPEN
      HAVE_SOCKETS
      HAVE_SANDBOX
      HAVE_STRFTIME
      HAVE_SENDFILE
      HAVE_UNISTD_H)
   (import
      (scheme core))
(begin
   (define OLVM                  #o1)
   (define OLVM_NOMAIN           #o2)
   (define OLVM_FFI              #o4)
   (define OLVM_CALLABLES       #o10)
   (define OLVM_INEXACTS        #o20)
   (define OLVM_BUILTIN_FMATH   #o40)
   (define SYSCALL_SYSINFO     #o100)
   ;                           #o200
   (define SYSCALL_MEMFD       #o400)
   (define SYSCALL_GETRLIMIT  #o1000)
   (define SYSCALL_GETRUSAGE  #o2000)
   (define HAS_UNSAFES       #o40000)
   (define HAVE_DLOPEN        #o10000)
   (define HAVE_SOCKETS       #o20000)
   (define HAVE_SANDBOX      #o100000)
   (define HAVE_STRFTIME     #o200000)
   (define HAVE_SENDFILE     #o400000)
   (define HAVE_UNISTD_H #o001000000)
   

   (define (vm:feature? ft)
      (not (eq? (vm:and (vm:features) ft) 0)))
))

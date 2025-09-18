(define-library (lib dbus)
   (export
      ; error
      make-DBusError
      dbus_error_init
      dbus_error_free
      dbus_error_is_set
      dbus_error_message ; * ol extension
      dbus_set_error

      ; connection
      dbus_connection_send_with_reply_and_block
      dbus_connection_flush
      dbus_connection_read_write
      dbus_connection_pop_message
      dbus_connection_unref
      dbus_connection_close

      ; message
      dbus_message_new_method_call
      dbus_message_unref
      dbus_message_is_signal
      dbus_message_get_type
         DBUS_MESSAGE_TYPE_INVALID
         DBUS_MESSAGE_TYPE_METHOD_CALL
         DBUS_MESSAGE_TYPE_METHOD_RETURN
         DBUS_MESSAGE_TYPE_ERROR
         DBUS_MESSAGE_TYPE_SIGNAL
      dbus_message_get_serial

      make-DBusMessageIter
      dbus_message_iter_init
      dbus_message_iter_next
      dbus_message_iter_get_arg_type
         DBUS_TYPE_INVALID
         DBUS_TYPE_BYTE
         DBUS_TYPE_BOOLEAN
         DBUS_TYPE_INT16
         DBUS_TYPE_UINT16
         DBUS_TYPE_INT32
         DBUS_TYPE_UINT32
         DBUS_TYPE_INT64
         DBUS_TYPE_UINT64
         DBUS_TYPE_DOUBLE
         DBUS_TYPE_STRING
         DBUS_TYPE_OBJECT_PATH
         DBUS_TYPE_SIGNATURE
         DBUS_TYPE_UNIX_FD
         DBUS_TYPE_ARRAY
         DBUS_TYPE_VARIANT
         DBUS_TYPE_STRUCT
         DBUS_TYPE_DICT_ENTRY
      dbus_message_iter_get_basic
      dbus_message_iter_recurse

      dbus_message_iter_init_append
      dbus_message_iter_append_basic
      dbus_message_iter_append_fixed_array

      dbus_message_iter_open_container
      dbus_message_iter_close_container

      ; bus
      dbus_bus_get
         DBUS_BUS_SESSION
         DBUS_BUS_SYSTEM
         DBUS_BUS_STARTER
      dbus_bus_add_match

      ; useful low level ffi functions
      & make-vptr
      vptr->string
      vptr->bool
      vptr->value
      fft-int16
   )

   (import
      (scheme core)
      (owl string) (owl ff)
      (otus ffi) (owl math))

(begin
   (setq DBUS (or
      (load-dynamic-library "libdbus.so")
      (load-dynamic-library "libdbus-1.so")
      (load-dynamic-library "libdbus-1.so.3")))

   (setq void fft-void)
   (setq bool fft-bool)
   (setq DBusBusType fft-enum)
   (setq int fft-int)
   (setq void* fft-void*)
   (setq void** (fft* void*))
   (setq vptr->string (lambda (x) ; cast vptr to the string
      (ffi nullptr (list type-string type-vptr) (list x))))

   (setq dbus_uint32_t fft-uint32)
   ;(setq dbus_unichar_t dbus_uint32_t)
   (setq dbus_bool_t dbus_uint32_t) ; boolean size must be fixed at 4 bytes due to wire protocol!

   ; helper function
   (define (& value)
      (cons
         (case (type value)
            (type-string (fft* type-string))
            (type-enum+  (fft* fft-int))
            (type-enum-  (fft* fft-int))
            (type-int+   (fft* fft-int))
            (type-int-   (fft* fft-int))
            (else #false))
         (box value)))

   ;; error
   (setq DBusError* type-vptr)
   (define (make-DBusError)
      (let* ((lo hi (vm:mul (size nullptr) 4)))
         (make-bytevector lo)))

   (define dbus_error_init (DBUS void "dbus_error_init" DBusError*))
   (define dbus_error_free (DBUS void "dbus_error_free" DBusError*))
   (define dbus_error_is_set (DBUS bool "dbus_error_is_set" DBusError*))
   (define dbus_error_message (lambda (err*)
      (define message* (bytevector->void* err* (size nullptr)))
      (vptr->string message*)))
   (define dbus_set_error (DBUS void "dbus_set_error" DBusError* type-string type-string #|...|#))

   ;; message
   (setq DBusMessage* type-vptr)
   (define dbus_message_new_method_call (DBUS DBusMessage* "dbus_message_new_method_call" type-string type-string type-string type-string))
   (define dbus_message_unref (DBUS void "dbus_message_unref" DBusMessage*))
   (define dbus_message_is_signal (DBUS bool "dbus_message_is_signal" DBusMessage* type-string type-string))
   (define dbus_message_get_type (DBUS int "dbus_message_get_type" DBusMessage*))
      (define DBUS_MESSAGE_TYPE_INVALID       0)
      (define DBUS_MESSAGE_TYPE_METHOD_CALL   1)
      (define DBUS_MESSAGE_TYPE_METHOD_RETURN 2)
      (define DBUS_MESSAGE_TYPE_ERROR         3)
      (define DBUS_MESSAGE_TYPE_SIGNAL        4)
   (define dbus_message_get_serial (DBUS dbus_uint32_t "dbus_message_get_serial" DBusMessage*))

   (setq DBusMessageIter* type-vptr)
   (define (make-DBusMessageIter)
      (make-bytevector 72))
   (define dbus_message_iter_init (DBUS bool "dbus_message_iter_init" DBusMessage* DBusMessageIter*))
   (define dbus_message_iter_next (DBUS bool "dbus_message_iter_next" DBusMessageIter*))
   (define dbus_message_iter_get_arg_type (DBUS int "dbus_message_iter_get_arg_type" DBusMessageIter*))
      (define DBUS_TYPE_INVALID       0)
      (define DBUS_TYPE_BYTE        #\y)
      (define DBUS_TYPE_BOOLEAN     #\b)
      (define DBUS_TYPE_INT16       #\n)
      (define DBUS_TYPE_UINT16      #\q)
      (define DBUS_TYPE_INT32       #\i)
      (define DBUS_TYPE_UINT32      #\u)
      (define DBUS_TYPE_INT64       #\x)
      (define DBUS_TYPE_UINT64      #\t)
      (define DBUS_TYPE_DOUBLE      #\d)
      (define DBUS_TYPE_STRING      #\s)
      (define DBUS_TYPE_OBJECT_PATH #\o)
      (define DBUS_TYPE_SIGNATURE   #\g)
      (define DBUS_TYPE_UNIX_FD     #\h)
      (define DBUS_TYPE_ARRAY       #\a)
      (define DBUS_TYPE_VARIANT     #\v)
      (define DBUS_TYPE_STRUCT      #\r)
      (define DBUS_TYPE_DICT_ENTRY  #\e)
   (define dbus_message_iter_get_basic (DBUS void "dbus_message_iter_get_basic" DBusMessageIter* void**))
   (define dbus_message_iter_recurse (DBUS void "dbus_message_iter_recurse" DBusMessageIter* DBusMessageIter*))

   (define dbus_message_iter_init_append (DBUS void "dbus_message_iter_init_append" DBusMessage* DBusMessageIter*))
   (define dbus_message_iter_append_basic (DBUS dbus_bool_t "dbus_message_iter_append_basic" DBusMessageIter* int fft-any))
   (define dbus_message_iter_append_fixed_array (DBUS dbus_bool_t "dbus_message_iter_append_fixed_array" DBusMessageIter* int void* int))

   (define dbus_message_iter_open_container (DBUS dbus_bool_t "dbus_message_iter_open_container" DBusMessageIter* int type-string DBusMessageIter*))
   (define dbus_message_iter_close_container (DBUS dbus_bool_t "dbus_message_iter_close_container" DBusMessageIter* DBusMessageIter*))


   ;; connection
   (setq DBusConnection* type-vptr)
   (define dbus_connection_send_with_reply_and_block (DBUS DBusMessage* "dbus_connection_send_with_reply_and_block" DBusConnection* DBusMessage* int DBusError*))
   (define dbus_connection_flush (DBUS void "dbus_connection_flush" DBusConnection*))
   (define dbus_connection_read_write (DBUS bool "dbus_connection_read_write" DBusConnection* int))
   (define dbus_connection_pop_message (DBUS DBusMessage* "dbus_connection_pop_message" DBusConnection*))
   (define dbus_connection_unref (DBUS void "dbus_connection_unref" DBusConnection*))
   (define dbus_connection_close (DBUS void "dbus_connection_close" DBusConnection*))

   ;; bus
   (define dbus_bus_get (DBUS DBusConnection* "dbus_bus_get" DBusBusType DBusError*))
      (define DBUS_BUS_SESSION 0)
      (define DBUS_BUS_SYSTEM 1)
      (define DBUS_BUS_STARTER 2)
   (define dbus_bus_add_match (DBUS void "dbus_bus_add_match" DBusConnection* type-string DBusError*))

))
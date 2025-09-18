(define-library (lib glib-2)
   (export
      void
      gboolean
      gpointer
      ;gconstpointer
      ;gchar guchar

      gint guint gshort gushort glong gulong
   ;  gint8 guint8 gint16 guint16 gint32 guint32 ; gint64 guint64
      
      gsize gssize goffset
      gfloat gdouble
      gchar*

      TRUE FALSE NULL
      g_free
      ;; get_type

      G_APPLICATION_FLAGS_NONE

      GCallback
      GClosureNotify
      GConnectFlags
      GApplication*
      GError*
      g_error_free

      ; c
      GObject*
      g_object_ref
      g_object_unref

      ; lisp
      GObject GObject?

      ; c
      GValue*
      make-GValue
      g_value_init
      g_value_get_gtype
      g_value_set_char
      g_value_get_char
      g_value_set_int
      g_value_get_int
      g_value_set_int64
      g_value_get_int64

      GDateTime*
      g_date_time_new_from_unix_utc
      g_date_time_format
      g_date_time_unref

      GType
      g_type_check_instance_is_a
      g_type_name_from_instance ; get type of widget
      g_type_name

      G_CALLBACK
      G_TYPE_GTYPE

      g_signal_connect
      g_signal_emit_by_name
      g_application_run
      g_application_quit

      g_object_get_data
      g_object_set_data

      ; todo: atomic pointers

      ; locale support
      gettext
      setlocale
         LC_ALL
      bindtextdomain
      bind_textdomain_codeset
      textdomain

      ; gslist
      GSList*
      g_slist_free
      g_slist_nth_data
      ;; g_slist_find_custom
      ;; GCompareFunc
   )
   (import
      (scheme core)
      (owl string) (owl ff)
      (otus ffi))

(begin
   (define TRUE 1)
   (define FALSE 0)
   (define NULL (vm:cast 0 fft-void*))

   (define G_APPLICATION_FLAGS_NONE 0)

   (define gint fft-int)
   (define guint fft-unsigned-int)
   (define gshort fft-short)
   (define gushort fft-unsigned-short)
   (define glong fft-long)
   (define gulong fft-unsigned-long)

   (define gpointer fft-void*)  (define gboolean fft-bool)
   (define gconstpointer fft-void*)

   (define gfloat fft-float)
   (define gdouble fft-double)

   (define gchar fft-char)
   (define gchar* type-string)
   (define gsize fft-unsigned-int)
   (define gssize fft-int)
   (define gint64 fft-signed-long-long)
   (define void fft-void)

   (define goffset gint64)

   (define GCallback type-callable) ; void (*GCallback)(void)
   (define GClosureNotify type-callable) ; void (*GClosureNotify)(gpointer, GClosure)
   (define GConnectFlags fft-int) ; enum
   (define GApplication* fft-void*)

   (define G_CALLBACK make-callback)
)
(cond-expand
   (Linux
      (begin
         (define GLIB (or
            (load-dynamic-library "libglib-2.0.so")
            (load-dynamic-library "libglib-2.0.so.0") ))
         (define GOBJECT (or
            (load-dynamic-library "libgobject-2.0.so")
            (load-dynamic-library "libgobject-2.0.so.0") ))
         (define GIO (or
            (load-dynamic-library "libgio-2.0.so")
            (load-dynamic-library "libgio-2.0.so.0") ))
      ))
   (Windows
      (begin
         (define GLIB
            (load-dynamic-library "libglib-2.0-0.dll"))
         (unless GLIB (runtime-error "Can't load libglib-2.0-0.dll"
            "try to install dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
         (define GOBJECT
            (load-dynamic-library "libgobject-2.0-0.dll"))
         (unless GOBJECT (runtime-error "Can't load libgobject-2.0-0.dll"
            "try to install dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
         (define GIO
            (load-dynamic-library "libgio-2.0-0.dll"))
         (unless GIO (runtime-error "Can't load libgio-2.0-0.dll"
            "try to install dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
      ))
)
; common part
(begin

(define g_free (GLIB void "g_free" gpointer))

(define GObject* type-vptr)
(define g_object_ref (GOBJECT gpointer "g_object_ref" gpointer))
(define g_object_unref (GOBJECT void "g_object_unref" gpointer))

(define gtag [])
(define (GObject this)
   (put this gtag #t))
(define (GObject? this)
   (get this gtag #f))


(define GError* type-vptr)
(define g_error_free (GOBJECT void "g_error_free" GError*))

(define g_signal_connect_data (GOBJECT gulong "g_signal_connect_data" gpointer type-string GCallback gpointer GClosureNotify GConnectFlags))
(define (g_signal_connect instance detailed_signal c_handler data)
   (g_signal_connect_data instance detailed_signal c_handler data #false 0))
(define g_signal_emit_by_name (GOBJECT void "g_signal_emit_by_name" gpointer type-string #|...|#))
(define g_application_run (GIO gint "g_application_run" GApplication* gint (fft* fft-void*)))
(define g_application_quit (GIO void "g_application_quit" GApplication*))

(define GDateTime* type-vptr)
(define g_date_time_new_from_unix_utc (GLIB GDateTime* "g_date_time_new_from_unix_utc" gint64))
(define g_date_time_format (GLIB type-vptr "g_date_time_format" GDateTime* gchar*))
(define g_date_time_unref (GLIB void "g_date_time_unref" GDateTime*))


(define g_object_get_data (GOBJECT gpointer "g_object_get_data" GObject* type-string))
(define g_object_set_data (GOBJECT void "g_object_set_data" GObject* gpointer))

; --=( locale support )=---------------
(define gettext (GLIB type-string "gettext" type-string))
(define setlocale (GLIB type-string "setlocale" fft-int type-string))
   (define LC_ALL 6)
(define bindtextdomain (GLIB type-string "bindtextdomain" type-string type-string))
(define bind_textdomain_codeset (GLIB type-string "bind_textdomain_codeset" type-string type-string))
(define textdomain (GLIB type-string "textdomain" type-string))


; --= ( gslist )=--------------
(define GSList* type-vptr)
;; (define GCompareFunc* type-vptr)
(define g_slist_free (GLIB void "g_slist_free" GSList*))
(define g_slist_nth_data (GLIB gpointer "g_slist_nth_data" GSList* guint))
;; (define g_slist_find_custom (GLIB GSList* "g_slist_find_custom" GSList* gconstpointer GCompareFunc*))
;; (define (GCompareFunc a b)
;;    #f)


; --=( GType )=---------------
(define GType fft-unsigned-long)
(define GTypeInstance* fft-void*)

(setq G_TYPE_FUNDAMENTAL_SHIFT 2)
(setq G_TYPE_MAKE_FUNDAMENTAL (lambda (x)
   (let* ((hi lo (vm:shl x G_TYPE_FUNDAMENTAL_SHIFT)))
      lo)))
;...
(setq G_TYPE_INT    (G_TYPE_MAKE_FUNDAMENTAL  6))
(setq G_TYPE_INT64  (G_TYPE_MAKE_FUNDAMENTAL 10))
;...
(setq G_TYPE_STRING (G_TYPE_MAKE_FUNDAMENTAL 16))
;...

(define G_TYPE_GTYPE ((GOBJECT GType "g_gtype_get_type"))) ; GIO?

(define g_type_check_instance_is_a (GOBJECT gboolean "g_type_check_instance_is_a" GTypeInstance* GType))
(define g_type_name_from_instance (GOBJECT gchar* "g_type_name_from_instance" GTypeInstance*))
(define g_type_name (GOBJECT gchar* "g_type_name" GType))


; --=( GValue )=---------
(define GValue* type-vptr)
(define g_value_init (GOBJECT GValue* "g_value_init" GValue* GType))
(define g_value_unset (GOBJECT void "g_value_unset" GValue*))

(define g_value_get_gtype (GOBJECT GType "g_value_get_gtype" GValue*))
; gchar
(define g_value_set_char (GOBJECT void "g_value_set_char" GValue* gchar))
(define g_value_get_char (GOBJECT gchar "g_value_get_char" GValue*))
; gint
(define g_value_set_int (GOBJECT void "g_value_set_int" GValue* gint))
(define g_value_get_int (GOBJECT gint "g_value_get_int" GValue*))
; gint64
(define g_value_set_int64 (GOBJECT void "g_value_set_int64" GValue* gint64))
(define g_value_get_int64 (GOBJECT gint64 "g_value_get_int64" GValue*))
; strings
(define g_value_set_string (GOBJECT void "g_value_set_string" GValue* gchar*))
(define g_value_get_string (GOBJECT gchar* "g_value_get_string" GValue*))

(define make-GValue
   (define (make) (make-bytevector 24)) ; GType(8) + gdouble(8)
   (case-lambda
      (()   (make))
      ((x)  (let ((v (make)))
               (cond
                  ((value? x)
                     (g_value_init v G_TYPE_INT)
                     (g_value_set_int v x))
                  ((integer? x)
                     (g_value_init v G_TYPE_INT64)
                     (g_value_set_int64 v x))
                  ((string? x)
                     (g_value_init v G_TYPE_STRING)
                     (g_value_set_string v x))
                  (else #f))
               v))))

))
   (define-library (lib gdk-3)
   (export
      GSourceFunc
      gdk_threads_add_idle
      gdk_threads_add_timeout

      gdk_x11_display_get_xdisplay
      gdk_x11_window_get_xid

      GdkGLContext*
      GdkDisplay*
      GdkWindow*

      ;; events
      GdkEvent*
      gdk_event_get_event_type
      gdk_event_get_coords

      ; lisp interface
      GdkEvent
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (lib glib-2))

(cond-expand
   (Linux
      (begin
         (define GDK (or
            (load-dynamic-library "libgdk-3.so")
            (load-dynamic-library "libgdk-3.so.0") ))
      ))
   (Windows
      (begin
         (define GDK (load-dynamic-library "libgdk-3-0.dll"))
         (unless GDK (runtime-error "Can't load libgdk-3-0.dll"
            "try to install dlls from https://github.com/yuriy-chumak/libol-gtk-3/releases/"))
      )) )

(begin
   (define GdkDisplay* type-vptr)
   (define GdkWindow* type-vptr)

   (define GSourceFunc type-callable)

   (define gdk_threads_add_idle (GDK guint "gdk_threads_add_idle" GSourceFunc gpointer))
   (define gdk_threads_add_timeout (GDK guint "gdk_threads_add_timeout" guint GSourceFunc gpointer))

   (define gdk_x11_display_get_xdisplay (GDK type-vptr "gdk_x11_display_get_xdisplay" type-vptr))
   (define gdk_x11_window_get_xid (GDK type-vptr "gdk_x11_window_get_xid" GdkWindow*))

   (define GdkGLContext* fft-void*)

   (define GdkEvent* type-vptr)
   (define GdkEventType type-value+)
   (define gdouble& (fft& gdouble))

   (define gdk_event_get_event_type (GDK GdkEventType "gdk_event_get_event_type" GdkEvent*))
   (define gdk_event_get_coords (GDK gboolean "gdk_event_get_coords" GdkEvent* gdouble& gdouble&))

   ; lisp interface
   (define GdkEvent
      (define (make ptr options)
         (define this {
            'ptr ptr ; raw pointer

            ; Retrieves the type of the event.
            'get-type (lambda ()
                  (gdk_event_get_event_type ptr))

            ; Extract the event window relative x/y coordinates from an event.
            'get-coords (lambda ()
                  (define x '(#i0))
                  (define y '(#i0))
                  (if (gdk_event_get_coords ptr x y)
                     (cons (car x) (car y))))

            ; internals
            'super #false
         })
         ; smart object
         (GObject this))
   ; main
   (case-lambda
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               (else
                  (runtime-error "GdkEvent: invalid argument" a1)) ))
   ))

))

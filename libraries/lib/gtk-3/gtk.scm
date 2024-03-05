(define-library (lib gtk-3 gtk)
   (export
      GTK3

      GtkCallback
      GTK_CALLBACK

      ; GtkAlign
      ; GtkArrowType
      ; GtkBaselinePosition
      ; GtkDeleteType
      ; GtkDirectionType
      ; GtkIconSize
      ; GtkSensitivityType
      ; GtkTextDirection
      ; GtkJustification
      ; GtkMenuDirectionType
      ; GtkMessageType
      ; GtkMovementStep
      ; GtkScrollStep
      GtkOrientation
      GTK_ORIENTATION_HORIZONTAL
      GTK_ORIENTATION_VERTICAL
      ; GtkPackType
      ; GtkPositionType
      ; GtkReliefStyle
      ; GtkScrollType
      ; GtkSelectionMode
      ; GtkShadowType
      ; GtkStateType
      ; GtkToolbarStyle
      ; GtkWrapMode
      ; GtkSortType
      ; GtkIMPreeditStyle
      ; GtkIMStatusStyle
      ; GtkPackDirection
      ; GtkPrintPages
      ; GtkPageSet
      ; GtkNumberUpLayout
      ; GtkPageOrientation
      ; GtkPrintQuality
      ; GtkPrintDuplex
      ; GtkUnit
      ; GtkTreeViewGridLines
      ; GtkDragResult
      ; GtkSizeGroupMode
      ; GtkSizeRequestMode
      ; GtkScrollablePolicy
      ; GtkStateFlags
      ; GtkRegionFlags
      ; GtkJunctionSides
      ; GtkBorderStyle
      ; GtkLevelBarMode
      ; GtkInputPurpose
      ; GtkInputHints
      ; GtkPropagationPhase
      ; GtkEventSequenceState
      ; GtkPanDirection
      ; GtkPopoverConstraint

      GtkStyleContext*

      ; lisp
      GtkThis GtkThis?

      (exports (lib glib-2)))
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (lib glib-2))

(cond-expand
   (Linux
      (begin
         (define GTK3 (load-dynamic-library "libgtk-3.so"))
      ))
   (Windows
      (begin
         (define GTK3 (load-dynamic-library "libgtk-3-0.dll"))
      )) )

(begin
   (define GtkOrientation gint)
   (define GTK_ORIENTATION_HORIZONTAL 0)
   (define GTK_ORIENTATION_VERTICAL 1)

   ; (lib gtk-3 style-context)
   (define GtkStyleContext* type-vptr)

   (define GtkCallback type-callable)
   (define-syntax GTK_CALLBACK
      (syntax-rules ()
         ((GTK_CALLBACK (userdata) . rest)
            (vm:pin (cons
               (cons gint (list gpointer))
               (lambda (userdata)
                  .rest))))
         ((GTK_CALLBACK (object userdata) . rest)
            (vm:pin (cons
               (cons gint (list GObject* gpointer))
               (lambda (object userdata)
                  .rest))))
         ((GTK_CALLBACK (object arg1 userdata) . rest)
            (vm:pin (cons
               (cons gint (list GObject* GObject* gpointer))
               (lambda (object arg1 userdata)
                  .rest))))
         ((GTK_CALLBACK (object arg1 arg2 userdata) . rest)
            (vm:pin (cons
               (cons gint (list GObject* GObject* GObject* gpointer))
               (lambda (object arg1 arg2 userdata)
                  .rest))))
      ))

   (define (GtkThis this)
      (put this 'gtk? #true))
      ;; (case-lambda
      ;;    (() this)
      ;;    ((key . args)
      ;;       (define value (this key #f))
      ;;       (if (function? value)
      ;;          (apply value args)
      ;;          value)) ))
   (define (GtkThis? this)
      (getf this 'gtk?))
      ;; ; todo: function? and arity may be 0 and (ff? (force))
      ;; (function? this))

   ;(define (make-GtkCallback)
   ; todo: make macro for make-GtkCallback


))
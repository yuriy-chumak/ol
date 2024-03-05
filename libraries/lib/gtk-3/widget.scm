(define-library (lib gtk-3 widget)
   (description "Base class for all widgets")
   (export
      GtkWidget*
      GTK_TYPE_WIDGET
      gtk_widget_get_type

      gtk_widget_show
      gtk_widget_show_all
      gtk_widget_hide
      gtk_widget_destroy
      gtk_widget_realize

      gtk_widget_grab_focus

      gtk_widget_is_sensitive
      gtk_widget_set_sensitive
      gtk_widget_get_sensitive

      gtk_widget_set_double_buffered
      gtk_widget_get_toplevel
      gtk_widget_get_display

      gtk_widget_get_style_context
      gtk_widget_get_window
      gtk_widget_get_allocated_width
      gtk_widget_get_allocated_height

      gtk_widget_queue_resize

      ; lisp
      GtkWidget
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (lib gdk-3)
      (lib gtk-3 gtk))

(begin
   (define GtkWidget* type-vptr)
   (define gtk_widget_get_type (GTK3 GType "gtk_widget_get_type"))
   (define GTK_TYPE_WIDGET (gtk_widget_get_type))

   (define gtk_widget_new (GTK3 GtkWidget* "gtk_widget_new" GType gchar* #|...|#))
   (define gtk_widget_destroy (GTK3 void "gtk_widget_destroy" GtkWidget*))
   ; gtk_widget_destroyed
   ; gtk_widget_unparent
   (define gtk_widget_show (GTK3 void "gtk_widget_show" GtkWidget*))
   (define gtk_widget_hide (GTK3 void "gtk_widget_hide" GtkWidget*))
   ; gtk_widget_show_now
   (define gtk_widget_show_all (GTK3 void "gtk_widget_show_all" GtkWidget*))
   ; gtk_widget_set_no_show_all
   ; gtk_widget_get_no_show_all
   ; gtk_widget_map
   ; gtk_widget_unmap
   (define gtk_widget_realize (GTK3 void "gtk_widget_realize" GtkWidget*))
   ; gtk_widget_unrealize
   ; gtk_widget_draw
   ; gtk_widget_queue_draw
   ; gtk_widget_queue_draw_area
   ; gtk_widget_queue_draw_region
   (define gtk_widget_queue_resize (GTK3 void "gtk_widget_queue_resize" GtkWidget*))
   ; gtk_widget_queue_resize_no_redraw
   ; gtk_widget_queue_allocate
   ; gtk_widget_get_frame_clock
   ; gtk_widget_size_request
   ; gtk_widget_size_allocate
   ; gtk_widget_size_allocate_with_baseline
   ; gtk_widget_get_request_mode
   ; gtk_widget_get_preferred_width
   ; gtk_widget_get_preferred_height_for_width
   ; gtk_widget_get_preferred_height
   ; gtk_widget_get_preferred_width_for_height
   ; gtk_widget_get_preferred_height_and_baseline_for_width
   ; gtk_widget_get_preferred_size
   ; gtk_widget_get_child_requisition
   ; gtk_widget_add_accelerator
   ; gtk_widget_remove_accelerator
   ; gtk_widget_set_accel_path
   ; gtk_widget_list_accel_closures
   ; gtk_widget_can_activate_accel
   ; gtk_widget_mnemonic_activate
   ; gtk_widget_event
   ; gtk_widget_send_expose
   ; gtk_widget_send_focus_change
   ; gtk_widget_activate
   ; gtk_widget_reparent
   ; gtk_widget_intersect
   ; gtk_widget_region_intersect
   ; gtk_widget_freeze_child_notify
   ; gtk_widget_child_notify
   ; gtk_widget_thaw_child_notify
   ; gtk_widget_set_can_focus
   ; gtk_widget_get_can_focus
   ; gtk_widget_has_focus
   ; gtk_widget_is_focus
   ; gtk_widget_has_visible_focus
   (define gtk_widget_grab_focus (GTK3 void "gtk_widget_grab_focus" GtkWidget*))
   ; gtk_widget_set_focus_on_click
   ; gtk_widget_get_focus_on_click
   ; gtk_widget_set_can_default
   ; gtk_widget_get_can_default
   ; gtk_widget_has_default
   ; gtk_widget_grab_default
   ; gtk_widget_set_receives_default
   ; gtk_widget_get_receives_default
   ; gtk_widget_has_grab
   ; gtk_widget_device_is_shadowed
   ; gtk_widget_set_name
   ; gtk_widget_get_name
   ; gtk_widget_set_state
   ; gtk_widget_get_state
   ; gtk_widget_set_state_flags
   ; gtk_widget_unset_state_flags
   ; gtk_widget_get_state_flags
   (define gtk_widget_set_sensitive (GTK3 void "gtk_widget_set_sensitive" GtkWidget* gboolean))
   (define gtk_widget_get_sensitive (GTK3 gboolean "gtk_widget_get_sensitive" GtkWidget*))
   (define gtk_widget_is_sensitive (GTK3 gboolean "gtk_widget_is_sensitive" GtkWidget*))  
   ; gtk_widget_set_visible
   ; gtk_widget_get_visible
   ; gtk_widget_is_visible
   ; gtk_widget_set_has_window
   ; gtk_widget_get_has_window
   ; gtk_widget_is_toplevel
   ; gtk_widget_is_drawable
   ; gtk_widget_set_realized
   ; gtk_widget_get_realized
   ; gtk_widget_set_mapped
   ; gtk_widget_get_mapped
   ; gtk_widget_set_app_paintable
   ; gtk_widget_get_app_paintable
   (define gtk_widget_set_double_buffered (GTK3 void "gtk_widget_set_double_buffered" GtkWidget* gboolean))
   ; gtk_widget_get_double_buffered
   ; gtk_widget_set_redraw_on_allocate
   ; gtk_widget_set_parent
   ; gtk_widget_get_parent
   ; gtk_widget_set_parent_window
   ; gtk_widget_get_parent_window
   ; gtk_widget_set_child_visible
   ; gtk_widget_get_child_visible
   ; gtk_widget_set_window
   (define gtk_widget_get_window (GTK3 GdkWindow* "gtk_widget_get_window" GtkWidget*))
   ; gtk_widget_register_window
   ; gtk_widget_unregister_window
   (define gtk_widget_get_allocated_width (GTK3 fft-int "gtk_widget_get_allocated_width" GtkWidget*))
   (define gtk_widget_get_allocated_height (GTK3 fft-int "gtk_widget_get_allocated_height" GtkWidget*))
   ; gtk_widget_get_allocated_baseline
   ; gtk_widget_get_allocated_size
   ; gtk_widget_get_allocation
   ; gtk_widget_set_allocation
   ; gtk_widget_set_clip
   ; gtk_widget_get_clip
   ; gtk_widget_get_requisition
   ; gtk_widget_child_focus
   ; gtk_widget_keynav_failed
   ; gtk_widget_error_bell
   ; gtk_widget_set_size_request
   ; gtk_widget_get_size_request
   ; gtk_widget_set_events
   ; gtk_widget_add_events
   ; gtk_widget_set_device_events
   ; gtk_widget_add_device_events
   ; gtk_widget_set_opacity
   ; gtk_widget_get_opacity
   ; gtk_widget_set_device_enabled
   ; gtk_widget_get_device_enabled
   (define gtk_widget_get_toplevel (GTK3 GtkWidget* "gtk_widget_get_toplevel" GtkWidget*))
   ; gtk_widget_get_ancestor
   ; gtk_widget_get_visual
   ; gtk_widget_set_visual
   ; gtk_widget_get_screen
   ; gtk_widget_has_screen
   ; gtk_widget_get_scale_factor
   (define gtk_widget_get_display (GTK3 GdkDisplay* "gtk_widget_get_display" GtkWidget*))
   ; gtk_widget_get_root_window
   ; gtk_widget_get_settings
   ; gtk_widget_get_clipboard
   ; gtk_widget_get_hexpand
   ; gtk_widget_set_hexpand
   ; gtk_widget_get_hexpand_set
   ; gtk_widget_set_hexpand_set
   ; gtk_widget_get_vexpand
   ; gtk_widget_set_vexpand
   ; gtk_widget_get_vexpand_set
   ; gtk_widget_set_vexpand_set
   ; gtk_widget_queue_compute_expand
   ; gtk_widget_compute_expand
   ; gtk_widget_get_support_multidevice
   ; gtk_widget_set_support_multidevice
   ; gtk_widget_class_set_accessible_type
   ; gtk_widget_class_set_accessible_role
   ; gtk_widget_get_accessible
   ; gtk_widget_get_halign
   ; gtk_widget_set_halign
   ; gtk_widget_get_valign
   ; gtk_widget_get_valign_with_baseline
   ; gtk_widget_set_valign
   ; gtk_widget_get_margin_left
   ; gtk_widget_set_margin_left
   ; gtk_widget_get_margin_right
   ; gtk_widget_set_margin_right
   ; gtk_widget_get_margin_start
   ; gtk_widget_set_margin_start
   ; gtk_widget_get_margin_end
   ; gtk_widget_set_margin_end
   ; gtk_widget_get_margin_top
   ; gtk_widget_set_margin_top
   ; gtk_widget_get_margin_bottom
   ; gtk_widget_set_margin_bottom
   ; gtk_widget_get_events
   ; gtk_widget_get_device_events
   ; gtk_widget_get_pointer
   ; gtk_widget_is_ancestor
   ; gtk_widget_translate_coordinates
   ; gtk_widget_hide_on_delete
   ; gtk_widget_override_color
   ; gtk_widget_override_background_color
   ; gtk_widget_override_font
   ; gtk_widget_override_symbolic_color
   ; gtk_widget_override_cursor
   ; gtk_widget_reset_style
   ; gtk_widget_create_pango_context
   ; gtk_widget_get_pango_context
   ; gtk_widget_set_font_options
   ; gtk_widget_get_font_options
   ; gtk_widget_create_pango_layout
   ; gtk_widget_render_icon_pixbuf
   ; gtk_widget_set_composite_name
   ; gtk_widget_get_composite_name
   ; gtk_widget_push_composite_child
   ; gtk_widget_pop_composite_child
   ; gtk_widget_class_install_style_property
   ; gtk_widget_class_install_style_property_parser
   ; gtk_widget_class_find_style_property
   ; gtk_widget_class_list_style_properties
   ; gtk_widget_style_get_property
   ; gtk_widget_style_get_valist
   ; gtk_widget_style_get
   ; gtk_widget_set_direction
   ; gtk_widget_get_direction
   ; gtk_widget_set_default_direction
   ; gtk_widget_get_default_direction
   ; gtk_widget_is_composited
   ; gtk_widget_shape_combine_region
   ; gtk_widget_input_shape_combine_region
   ; gtk_widget_list_mnemonic_labels
   ; gtk_widget_add_mnemonic_label
   ; gtk_widget_remove_mnemonic_label
   ; gtk_widget_set_tooltip_window
   ; gtk_widget_get_tooltip_window
   ; gtk_widget_trigger_tooltip_query
   ; gtk_widget_set_tooltip_text
   ; gtk_widget_get_tooltip_text
   ; gtk_widget_set_tooltip_markup
   ; gtk_widget_get_tooltip_markup
   ; gtk_widget_set_has_tooltip
   ; gtk_widget_get_has_tooltip
   ; gtk_cairo_should_draw_window
   ; gtk_cairo_transform_to_window
   ; gtk_requisition_get_type
   ; gtk_requisition_new
   ; gtk_requisition_copy
   ; gtk_requisition_free
   ; gtk_widget_in_destruction
   (define gtk_widget_get_style_context (GTK3 GtkStyleContext* "gtk_widget_get_style_context" GtkWidget*))
   ; gtk_widget_get_path
   ; gtk_widget_class_set_css_name
   ; gtk_widget_class_get_css_name
   ; gtk_widget_get_modifier_mask
   ; gtk_widget_insert_action_group
   ; gtk_widget_add_tick_callback
   ; gtk_widget_remove_tick_callback
   ; gtk_widget_class_bind_template_callback
   ; gtk_widget_class_bind_template_child
   ; gtk_widget_class_bind_template_child_internal
   ; gtk_widget_class_bind_template_child_private
   ; gtk_widget_class_bind_template_child_internal_private
   ; gtk_widget_init_template
   ; gtk_widget_get_template_child
   ; gtk_widget_class_set_template
   ; gtk_widget_class_set_template_from_resource
   ; gtk_widget_class_bind_template_callback_full
   ; gtk_widget_class_set_connect_func
   ; gtk_widget_class_bind_template_child_full
   ; gtk_widget_get_action_group
   ; gtk_widget_list_action_prefixes
   ; gtk_widget_set_font_map
   ; gtk_widget_get_font_map

   (define GtkWidget
      (define (make ptr properties)
         (define this {
            'ptr ptr ; raw pointer
            'widget ptr ; same pointer

            ; Recursively shows a widget, and any child widgets.
            ; (todo: move to "GtkWidget")
            'show-all (lambda ()
               (gtk_widget_show_all ptr))

            ; Signals that all holders of a reference to the widget should release the reference that they hold.
            'set-destroy-handler (lambda (handler)
               (define callback
                  (cond
                     ((eq? (type handler) type-callable) ; callback
                        handler)
                     ((and (eq? (type handler) type-enum+) ; pin?
                           (pair? (vm:deref handler))
                           (function? (cdr (vm:deref handler))))
                        (G_CALLBACK handler))
                     ((function? handler)
                        (G_CALLBACK
                           (vm:pin (cons
                              (cons gint (list GtkWidget* type-vptr))
                              (lambda (widget userdata)
                                 (handler (make widget #false)))))))
                     (else
                        (runtime-error "GtkWindow" "invalid handler"))))
               (g_signal_connect ptr "destroy" callback #f))

            'super #false
            'setup (lambda (this options)
               (if (options 'on-destroy #f)
                  ((this 'set-destroy-handler) (options 'on-destroy)))

               #true)
         })
         ; no options handling yet
         (GtkThis this))
   ; main
   (case-lambda
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #f))
               (else
                  (runtime-error "GtkWidget: invalid argument" a1)) ))
      ((a1 . pr) (cond
               ((integer? a1) ; GType
                  (make (apply gtk_widget_new (cons a1 pr)) #f))
               (else
                  (runtime-error "GtkWidget: invalid arguments" (cons a1 pr))) ))
   ))
))

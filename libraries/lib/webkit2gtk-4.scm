(define-library (lib webkit2gtk-4)
   (export

      webkit_web_view_new
      webkit_web_view_load_uri
      webkit_web_view_load_html

      webkit_web_view_evaluate_javascript
      webkit_web_view_get_user_content_manager
      webkit_user_content_manager_register_script_message_handler

      ; javascript support
      webkit_javascript_result_get_global_context
      webkit_javascript_result_get_value
      JSValueIsString
      JSValueToStringCopy
      JSStringGetMaximumUTF8CStringSize
      JSStringGetUTF8CString
      JSStringRelease
   )

   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define WEBKIT2 (or
      (load-dynamic-library "libwebkit2gtk-4.0.so")
      (load-dynamic-library "libwebkit2gtk-4.0.so.37")))

   (define WebKitWebView* type-vptr)

   (define GCancellable* (fft& GtkCallback))
   (define GAsyncReadyCallback GtkCallback)

   (define webkit_web_view_new (WEBKIT2 GtkWidget* "webkit_web_view_new"))
   (define webkit_web_view_load_uri (WEBKIT2 fft-void "webkit_web_view_load_uri" WebKitWebView* type-string))
   (define webkit_web_view_load_html (WEBKIT2 fft-void "webkit_web_view_load_html" WebKitWebView* type-string type-string))
   ; webkit_web_view_is_loading
   ; webkit_web_view_reload
   ; webkit_web_view_reload_bypass_cache
   ; webkit_web_view_get_estimated_load_progress
   ; webkit_web_view_go_back
   (define webkit_web_view_evaluate_javascript (WEBKIT2 fft-void "webkit_web_view_evaluate_javascript"
      WebKitWebView*
      type-string gssize
      type-string
      type-string
      GCancellable*
      GAsyncReadyCallback
      fft-any))

   (define WebKitUserContentManager* type-vptr)
   (define webkit_web_view_get_user_content_manager (WEBKIT2 WebKitUserContentManager* "webkit_web_view_get_user_content_manager" WebKitWebView*))
   (define webkit_user_content_manager_register_script_message_handler (WEBKIT2 gboolean "webkit_user_content_manager_register_script_message_handler" WebKitUserContentManager* type-string))

   ; javascript
   (define JSValueRef type-vptr)
   (define JSGlobalContextRef type-vptr)
   (define JSContextRef JSGlobalContextRef)
   (define JSStringRef type-vptr)
   (define JSValueRef* (fft& JSValueRef))
   (define WebKitJavascriptResult* type-vptr)

   (define webkit_javascript_result_get_global_context (WEBKIT2 JSGlobalContextRef "webkit_javascript_result_get_global_context" WebKitJavascriptResult*))
   (define webkit_javascript_result_get_value (WEBKIT2 JSValueRef "webkit_javascript_result_get_value" WebKitJavascriptResult*))

   (define JSValueIsString (WEBKIT2 fft-bool "JSValueIsString" JSContextRef JSValueRef))
   (define JSValueToStringCopy (WEBKIT2 JSStringRef "JSValueToStringCopy" JSContextRef JSValueRef JSValueRef*))
   (define JSStringGetMaximumUTF8CStringSize (WEBKIT2 fft-size_t "JSStringGetMaximumUTF8CStringSize" JSStringRef))
   (define JSStringGetUTF8CString (WEBKIT2 fft-size_t "JSStringGetUTF8CString" JSStringRef fft-void* fft-size_t))
   (define JSStringRelease (WEBKIT2 fft-void "JSStringRelease" JSStringRef))


))

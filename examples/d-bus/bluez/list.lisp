#!/usr/bin/env ol
(import (lib dbus))

(define RED "\e[0;31m")
(define GREEN "\e[0;32m")
(define YELLOW "\e[0;33m")
(define END "\e[0;0m")

; helper functions:
; get the property value
(define (Value reply)
   (define &iter (make-DBusMessageIter))
   (dbus_message_iter_init reply &iter)

   (define (get-Value &iter)
      (case (dbus_message_iter_get_arg_type &iter)
         (DBUS_TYPE_STRING
               (define reply_string (make-vptr))
               (dbus_message_iter_get_basic &iter reply_string)
               (vptr->string reply_string))
         (DBUS_TYPE_BOOLEAN
               (define reply_boolean (make-vptr))
               (dbus_message_iter_get_basic &iter reply_boolean)
               (vptr->bool reply_boolean))
         (DBUS_TYPE_INT16
               (define reply_int16 (make-vptr))
               (dbus_message_iter_get_basic &iter reply_int16)
               (vptr->value reply_int16 fft-int16))

         (DBUS_TYPE_VARIANT
               (define &viter (make-DBusMessageIter))
               (dbus_message_iter_recurse &iter &viter)
               (get-Value &viter))
         (else #f)))
   (get-Value &iter))

; read the property
(define (Get-Property Connection Folder Interface Property)
   (define &error (make-DBusError))
   (dbus_error_init &error) ; reset error before calls
   (define message (dbus_message_new_method_call "org.bluez" Folder "org.freedesktop.DBus.Properties" "Get"))
   (unless message
      (raise "DBus message allocation error"))
   (define &iter (make-DBusMessageIter))
   (dbus_message_iter_init_append message &iter)
   (dbus_message_iter_append_basic &iter DBUS_TYPE_STRING (& Interface))
   (dbus_message_iter_append_basic &iter DBUS_TYPE_STRING (& Property))
   (define reply (dbus_connection_send_with_reply_and_block Connection message -1 &error))

   (dbus_message_unref message)
   (when reply
      (define out (Value reply))
      (dbus_message_unref reply)
      out))


; -- main ----------------------
(define &error (make-DBusError))
(dbus_error_init &error)

(with-exception-handler
   (lambda (exception)
      (display exception)
      (if (dbus_error_is_set &error)
      then
         (print ":")
         (print "  " (dbus_error_message &error))
      else
         (print))
      (dbus_error_free &error)
      (exit 1))
   (lambda ()
      ; connection
      (define connection (dbus_bus_get DBUS_BUS_SYSTEM &error))
      (if (dbus_error_is_set &error)
         (raise "DBus connection error"))

      (define message (dbus_message_new_method_call "org.bluez" "/" "org.freedesktop.DBus.ObjectManager" "GetManagedObjects"))
      (unless message
         (raise "DBus message allocation error"))
      (define reply (dbus_connection_send_with_reply_and_block connection message -1 &error))
      (dbus_message_unref message)
      (if (dbus_error_is_set &error)
         (raise "message error"))

      (define &iter (make-DBusMessageIter))
      (dbus_message_iter_init reply &iter)
      (define &array_iter (make-DBusMessageIter))
      (dbus_message_iter_recurse &iter &array_iter)

      (define &dict_iter (make-DBusMessageIter))
      (let loop ()
         (unless (= (dbus_message_iter_get_arg_type &array_iter) DBUS_TYPE_INVALID)
            (dbus_message_iter_recurse &array_iter &dict_iter)
            (define str (make-vptr))
            (dbus_message_iter_get_basic &dict_iter str)
            (print (vptr->string str))

            (define folder (vptr->string str))
            (define name (Get-Property connection folder "org.bluez.Device1" "Name"))
            (when name
               (print "  Name: " GREEN name END)
               (for-each (lambda (property)
                     (for-each display (list "  " property ": "))
                     (let ((value (Get-Property connection folder "org.bluez.Device1" property)))
                        (if value (print YELLOW value END) (print #f))))
                  '("Connected" "Address" "AddressType" "TxPower" "RSSI")))

            (dbus_message_iter_next &array_iter)
            (loop)))

      (dbus_message_unref reply)
      (dbus_connection_unref connection)
      #true))

(print "done.")

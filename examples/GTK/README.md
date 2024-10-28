TOC
---

1. [Installation](#installation)
1. [Getting started](#getting-started)
   1. [Window](#window)
   1. [Customized Window](#customized-window)
   1. [Window With Content](#window-with-content)
   1. [Multithreaded Window](#multithreaded-window)
1. [Glade]
   1. [Glade](#glade-1)
   1. [Multithreaded Glade](#multithreaded-glade)
1. [Event]
   1. [Glade Events](#glade-events)
1. [Label]
   1. [Labels and Strings](#labels-and-strings)


Installation
============

You must have a gtk-3 library installed.


Getting started
===============

Import glib and gtk-3 libraries.

```scheme
(import
   (lib glib-2)
   (lib gtk-3))
```

Window
------

In the simplest case we can create an application and a window manually.

```scheme
;; application activate
(define (activate appl)
   ; create default window with a title
   (define window (GtkWindow appl "Window"))

   ; show it
   ((window 'show-all)))

;; create an application
(define app (GtkApplication {
   'on-activate activate
}))

;; run
((app 'run) (command-line))
```
([source code](1.0.Window.lisp))


Customized Window
-----------------

We can change window creation to be more customized. Let's change window initial dimensions and window icon.
```scheme
   (define window (GtkWindow appl {
      'title "Customized Window"
      'width 640 'height 360
      'icon "dialog-information"
   }))
```
([source code](1.1.Window.Customized.lisp))


Window with Content
-------------------

We can add any widget to the window (or to any container).
```scheme
   (define label (GtkLabel
      "Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit."))
   ((window 'add) label)
```
([source code](1.2.Window.With.Content.lisp))


Multithreaded Window
--------------------

We must to take a few extra steps to make our application multi-threaded.
This is required for async functions support.

First of all we must notify our application about multithreaded environment (the *'multithreaded* flag):
```scheme
(define app (GtkApplication {
   'multithreaded #true
   ...
}))
```

Plus we must write proper exit code (shutdown all running threads):
```scheme
   (define window (GtkWindow appl {
      ...
      ; properly stop running threads
      'on-destroy (lambda (this)
         (for-each kill (running-threads))
         (g_application_quit (gtk_window_get_application (this 'widget))))
   }))
```

And voila, now we can create any async functions, any actors, and other multithreaded things.
```scheme
   (async (lambda ()
      (let infinity-loop ()
         ; do something...
         (wait 1000)
         (infinity-loop))))
```
([source code](1.3.Window.Multithreaded.lisp))


Glade
=====

Glade is a graphical user interface builder for GTK, an excellent interface designer.

Steps below will use next [xml template](2.0.Glade.glade) created using Glade 3.38.2.


Glade
-----

If we want to use Glade templates as the main UI, we need to change the creation steps.
No longer need to manually create application instance and the window, but take widgets from a template and wrap them into an internal object model.

Please note that we must call `(gtk_main_quit)` instead of `(g_application_quit)`, because we do `(gtk_main)` instead of `((app 'run))`.

```scheme
;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "2.0.Glade.glade"))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'on-destroy (lambda (this)
         (gtk_main_quit))
   }))

;; show it
((window 'show-all))

;; run
(gtk_main)
```
([source code](2.0.Glade.lisp))


Multithreaded Glade
-------------------

Same steps as before: the flag and proper application exit:
```scheme
(gtk_init {
   'multithreaded #true
})

...

(define window (GtkWindow
   ((builder 'get-object) "window") {
      'title "Glade Multithreaded Example"
      'on-destroy (lambda (this)
         ; properly stop running threads
         (for-each kill (running-threads))
         (gtk_main_quit))
   }))

```
([source code](2.1.Glade.Multithreaded.lisp))


Event
=====

Glade Events
------------

Glade allows us name required events without naming appropriate widgets, and assign multiple similar events from multiple widgets to single handler.

```scheme
; handle mouse button click and show the message box with coordinates as text
((builder 'add-callback-symbol) "mouse-clicked" (GTK_CALLBACK (widget event)
   (define x '(#i0))
   (define y '(#i0))
   (if (gdk_event_get_coords event x y)
      (GtkMessageDialog (gtk_widget_get_toplevel widget) {
         'type  GTK_MESSAGE_INFO
         'message (string-append
               "mouse clicked at"  "\n"
               (number->string (car x)) ", " (number->string (car y)))
      }))
   TRUE))
...
; notify builder that we are finished with assigning event handlers
((builder 'connect-signals))
```
([source code](3.0.Events.lisp))


Label
=====

Labels and Strings
------------------

You can use unicode strings freely. This example makes a string reverse from clicked label.

```scheme
((builder 'add-callback-symbol) "clicked" (GTK_CALLBACK (widget event userdata)
   (define label (GtkLabel userdata))
   ((output-label 'set-text) (string-append
      "Reversed string:\n\t"
      (list->string (reverse (string->list ((label 'get-text)))))))
   TRUE))
```
([source code](4.0.Labels.And.Strings.lisp))


---
credits:
* https://docs.gtk.org/gtk3/getting_started.html
* https://python-gtk-3-tutorial.readthedocs.io/en/latest/

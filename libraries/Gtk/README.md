# Gtk-3

#Super Functions
--------------------------------------------------------------

### Gtk:init
`(Gtk:init)`, default init  
`(Gtk:init {options})`, init with options  
`(Gtk:init argc argv)`, raw init with argc argv

#### options
```scheme
{
   'argv <list of raw gtk string arguments>
   'multithreaded <boolean>
}

```

### Gtk:main

### Gtk:quit

GtkApplication (no base class)
--------------------------------------------------------------
`(GtkApplication)`, default init  
`(GtkApplication #<vptr>)`, wrap raw gtk handle into GtkApplication object  
`(GtkApplication "name")`, create new GtkApplication with app *name*  
`(GtkApplication integer)`, create new GtkApplication with app name 'org.gtk.example' and *integer* gtk flags  
`(GtkApplication {options})`, create new GtkApplication with options *options*  
`(GtkApplication "name" integer)`, create new GtkApplication with app name *name* and flags integer  
`(GtkApplication integer "name")`, create new GtkApplication with app name *name* and flags integer

### options
```scheme
{
   'id <string name> ; app name
   'flags <integer> ; gtk flags
   'on-activate <activate handler>
   'multithreaded <boolean>
}
```

### GtkApplication members
```scheme
{
   'class 'Application
   'Ptr* #<vptr>
   'Application #<vptr>

   'run (lambda (command-line) ...)
   'quit (lambda () ...)

   'set-activate-handler (lambda () ...)
}
```

GtkWindow (base on GtkContainer)
--------------------------------------------------------------

### options
```scheme
{
   'application
   'title
   'width
   'height
   'icon

   ; GtkWidget
   'on-destroy
   'on-button-press
}
```

### members
```scheme
{
   'class 'Window
   'superclass 'Container
   'super #<GtkContainer>

   'Window #<vptr>

   'set-title ...
   'set-default-size ...
   'set-icon ...
   'get-application (lambda () ...) ; Getter
   'set-application ...

   ; GtkContainer:
   'Container #<vptr>
   'add (lambda (widget) ...)

   ; GtkWidget:
   'Ptr* #<vptr>
   'Widget #<vptr>

   'show-all (lambda () ...)
   'get-toplevel
   'set-destroy-handler
      ; StyleContext manipulations
   'add-provider
   'add-class
   'remove-class
   'has-class?
   'add-css
   'enable
   'disable

}
```

GtkContainer (base on GtkWidget)
--------------------------------------------------------------

GtkLabel (base on GtkWidget)
--------------------------------------------------------------

### options
```scheme
{
   'text #<string>
   'markup #<string>
}
```

### members
```scheme
{
   'class 'Label
   'superclass 'Widget
   'super #<GtkWidget>

   'Label #<vptr>

   'get-text
   'set-text
   'text ; get-text + set-text

   'set-markup

}
```
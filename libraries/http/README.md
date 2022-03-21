Simple HTTP Server
==================

Ol comes with a built-in simple HTTP Server, which in other words is a simple HTTP server that gives you standard GET handlers. This module can turn any directory of your system into a web server.

Requirements
------------

* `ls`

Usage
-----

Default port is 4000.

```$ echo ,load http/server| ol```

Or you can declare your own port number.

```$ echo ,load http/server| ol - --port 80```


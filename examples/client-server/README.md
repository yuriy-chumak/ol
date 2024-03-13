Client-Server example
=====================

server
------

* Listen port 8888.
* Execute a command [MOVE delta-x delta-y] which permanently moves a 'hero position.
* Answer new 'hero position (after move)
* Correctly process simultaneous clients access

client
------
* Connect to the server
* Wait for a greeting (and print)
* Send a [MOVE random random] command
* Print a new hero position
* Disconnect

Usage
-----

```bash
# first terminal:
$ ol server.lisp
```
```bash
# second terminal:
$ ol client.lisp
```

Example
-------

```bash
# first terminal:
$ ol server.lisp
server binded to 8888
server listening to 0.0.0.0:8888
client (127.0.0.1 . 56974) connected.
  received a command #(1001 1 1)
  hero answered moved
  current hero status is #ff((x . 11) (y . 8))
client (127.0.0.1 . 56976) connected.
  received a command #(1001 -1 0)
  hero answered moved
  current hero status is #ff((x . 10) (y . 8))
client (127.0.0.1 . 56978) connected.
  received a command #(1001 -2 0)
  hero answered moved
  current hero status is #ff((x . 8) (y . 8))
client (127.0.0.1 . 56980) connected.
  received a command #(1001 0 0)
  hero answered moved
  current hero status is #ff((x . 8) (y . 8))
client (127.0.0.1 . 56982) connected.
  received a command #(1001 2 1)
  hero answered moved
  current hero status is #ff((x . 10) (y . 9))
^C
```

```bash
# second terminal
$ ol client.lisp
server connected to a 127.0.0.1:8888
greeting: I see you, please wait...
  sending command (1 2 3 0 0 233 7 0 0 1 0 0 1 0)
  hero position is: #ff((x . 11) (y . 8))
done.
$ ol client.lisp
server connected to a 127.0.0.1:8888
greeting: I see you, please wait...
  sending command (1 2 3 0 0 233 7 0 32 1 0 0 0 0)
  hero position is: #ff((x . 10) (y . 8))
done.
$ ol client.lisp
server connected to a 127.0.0.1:8888
greeting: I see you, please wait...
  sending command (1 2 3 0 0 233 7 0 32 2 0 0 0 0)
  hero position is: #ff((x . 8) (y . 8))
done.
$ ol client.lisp
server connected to a 127.0.0.1:8888
greeting: I see you, please wait...
  sending command (1 2 3 0 0 233 7 0 0 0 0 0 0 0)
  hero position is: #ff((x . 8) (y . 8))
done.
$ ol client.lisp
server connected to a 127.0.0.1:8888
greeting: I see you, please wait...
  sending command (1 2 3 0 0 233 7 0 0 2 0 0 1 0)
  hero position is: #ff((x . 10) (y . 9))
done.
```
```
                  small,
        `___`         embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     functional!
   O t u s L i s p
```
*Based on Aki Helin's [Owl-Lisp](https://gitlab.com/owl-lisp/owl)*

CHANGELOG
=========

2.6 changelog:
 * [!] large write/print upgrade
 * [!] large regex update
 * [!] Now Ol (but not Olvm) can handle pipes.
 * regex update with "global" grab
 * `g//` regex returns string if arg was a string
 * dlopen/dlsym moved to base library
 * new `each` function
 * `=` now can compare not only numbers, but strings, vectors, etc.,
 * lot of documentation added
 * `runtime-error` goes more universal
 * `http/server` mime-types support added, can handle links with "?"
 * regex library upgraded
 * port printing updated
 * fix for long port numbers bug
 * pvenv now run repl if no './main' provided
 * print and write now prints regexps in source form
 * 258 and 261 olvm "crash" renamed to proper "error"
 * gtk-3 oop added
 * write strings fixed (for "\\")
 * print updated with #ff()
 * #ff(...) notation added
 * #u8(...) notation added
 * new function `write-simple`
 * win32 ol.exe made portable


### Otus Lisp, Version 2.5
2.5 changelog:
 * FFI structures-by-value in parameters,
 * FFI structures writeback,
 * new `infix-notation` macro,
 * `\` became a usual character, so can be used as part of symbol names,
 * GC speedup (growing strategy updated),
 * PVENV (PriVate ENVironment) or *portable* version added (as "tar" extensions, something like flatpak or appimage),
 * `string-ref` dramatically optimized,
 * *inexact numbers* printing updated,
 * *decimal fractions* (1.1, 2.0, etc.) in library names,
 * `system` renamed to `execvp`, new `system` provided,
 * `vector-map` twice speedup,
 * fixed issue with *port blocking* after Ol closed,
 * `parameterize` with `current-xx-port` upgraded,
 * new `WebKitGTK` library `(webkit2gtk-4)`,
 * new `(file glTF)` library,
 * `(OpenGL version-X-X)` changed to `(OpenGL X.X)`,
 * *base64 decode* fixed,
 * *ffi sizeof* reworked (now support structures, became native),
 * *ffi speedup* - type-vptr accept `(pair bytevector . offset)`,
 * '~' as a "default" path in "--home=" command option,
 * removed 'xxd' from the build requirements,
 * *repl* is no more an actor, but just a coroutine,
 * `(lib json)` returns inexact number if number with decimal dot,
 * *HAS_XXX* -> *HAVE_XXX* build variables renames,
 * default ffi values (#f -> 0),
 * floating-point ffi storing refactored,
 * `open-output-string` enabled for macos and win32,
 * pipes is no more optional, because definitely multiplatform,
 * `port->string` and `file->string` new functions,
 * basic WebSocket support in http library,
 * fft-any for all strings sends as utf-8 char*,
 * win32 compat updates,
 * macos compat updates, 
 * regression tests updated,
 * deprecated fft-size-t changed to actual fft-size_t,
 * glib/gtk update with gettext,
 * ffi bool fix,
 * seccomp and sandbox definitions updated,
 * "system" now supports #f instead of port (as "no port provided"),
 * json "stringify" added,
 * `,expand` updated (better output),
 * M1 ffi updated (very much arguments support),
 * aarch64 ffi fix for more than 8 integer arguments,
 * internal change for 'crashed -> 'crash,
 * new public function (divide a b) = a/b,
 * `list-copy` made r7rs-compliant,
 * `file-info` accepts both port and filename,
 * ceiling/truncate/round updated (added inexact numbers, fixed bug),
 * `sqrt` extended to the complex argument,
 * sqlite inexact numbers support,
 * EGL 1.1-1.5 added,
 * not getting stuck when trying to include invalid library (owl backport),
 * bugs and issues fixes,
 * more samples, more tests, more docs.


### Otus Lisp, Version 2.4 RC1

2.4 changelog:
 * `define-macro`,
 * tbd.

2.3 changelog:
 * Ol got it's own [twitter account](https://twitter.com/otus_lisp)
 * vm opcodes 3,4,6,7 (OCLOS, CLOS1) is deprecated, will be removed soon
 * new opcode 48 (universal CLOS) is introduced (will be changed to 3 after deprecations remove)
 * new 'vm:set!' vm command, should speedup internal bytevector manipulations
 * integrated disassembler introduced (use ",dis" repl command)
   * try ',dis +' to view the '+' function disassembly
 * ',save' repl feature returned; still experimental due to ffi
   * you can ',save' a current session in the binary file and return to saved state by starting new Ol session with this file
 * Unicode support updated to version 14.0.0
 * big numbers vm loader fix
 * windows setup update
 * GC tune (reduced full gc count)
 * more samples and tests
   * more gtk
   * 3d game shaders for beginners
   * convey's life
   * etc.
 * windows command line support multiple path in "--home=...;..."
 * (define-values) can be used inside lambdas, begin, etc.
 * (otus async) async/await/coroutine/sleep functions: new old otus feature
   * 'interact' is deprecated, use (await (mail ...)) instead
 * more r7rs compatibility: (features) got 'posix' symbol
 * i/o speedup (removed sleeper thread)
 * utf8-decode became a lazy, file reading speedup
 * json file reading: exponent issue fixed
 * 'string->number' became r7rs compliant
 * 'raise' and 'raise-continuable' introduced
 * etc.

### Otus Lisp, Version 2.2.1

2.2.1 changelog:
 * the build command line changed. See the "BUILD" section.
 * fasl format updated:
   * fasl is fully 32/64 bit independent (32-bit machines can execute 64-bit fasl and vice versa),
   * numbers encoded as numbers, not as objects,
   * big endian numbers order changed to little-endian
   * introduced a new object type 63 - "constructor", constructors are automatically executed by olvm during fasl loading process
     * if no constructors are found, the vanilla behavior will be used
     * note: for example, the (owl math) library contains a constructor named `math-constructor` that recalculates a native binary NaN value which is different under different FPU architectures.
  * restored and adapted REPL command ",save". Can be used to save the current REPL state to be continued after a pause on the same or different PC (even with a different architecture, including bit count and/or byte order).

```
                  small,
        `___`         embeddable
        (O,O)             and
        \)  )          purely
      ---"-"---     functional!
   O t u s L i s p

 /Based on Aki Helin's Owl-Lisp/
```

CHANGELOG
=========

### Otus Lisp, Version 2.3 RC2

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

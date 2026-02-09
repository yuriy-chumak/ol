BINARY SCRIPTS
--------------

OL can compile and execute precompiled programs.
While text-based lisp programs require compiler binary (450KB), libraries, and a (small) compilation time,
binary code requires only olvm (60K w/o, 90K with FFI) and is ready for immediate execution.  
Please keep in mind that FFI-related code must be initialized within the compiled code, not during compilation.

Ol 2.7 supports compilation from the command line.
Set `-c` (or `--compile`) option and define output file name with `-o=filename` (you can skip -o option, then stdout will be used),
and the script result will be compiled in binary form. For Ol 2.6, use `fasl-save` function.

```shell
$ echo "#true" |ol -c |xxd -g1
00000000: 00 0d 01                                         ...

$ echo 7 |ol -c |xxd -g1
00000000: 00 00 07                                         ...

$ echo "(lambda () 7)" |ol -c |xxd -g1
00000000: 02 10 0a 0b 01 00 05 0e 07 04 18 04 11 00        ..............

$ echo [1 2 3] |ol -c |xxd -g1
00000000: 01 02 03 00 00 01 00 00 02 00 00 03 00           .............
```

If you want to compile not just a function, but a full-fledged binary program with full language features, such as interned symbols and threads use `--entry` command line option.  
Note that your compiling function must be a variadric procedure in such case.

### example 1

Create `template.scm` with next content:
```scheme
; put any preparation steps here
(print-to stderr "compiling...")

; this will be saved as compiled output since
; it is the result of script execution (last s-exp)
(lambda args
   (print "arguments: " args)

   (define out
   (let faktr ((x (if (null? args)
                     13
                     (string->number (first args)))))
      (if (= x 1)
         1
         (* x (faktr (- x 1))))))

   (print "factorial: " out)

   ; return execution result (let it be number of digits in the out)
   (ilog 10 out))
```

Let's compile and check the output (`--entry` option must come before the filename, otherwise it will be passed to the file as a param instead of to compiler).
```bash
$ ol --compile --entry template.scm >a.bin
compiling...

#   or
# ol -c -o=a.bin --entry template.scm
#   or
# cat template.scm |ol --entry >a.bin

$ ls -l a.bin
-rw------- 1 user user 92574 Jan 28 12:20 a.bin

$ xxd ./a.bin
00000000: 0203 066c 616d 6264 6102 0304 636f 6e73  ...lambda...cons
00000010: 0203 0363 6172 0203 0363 6472 0203 0372  ...car...cdr...r
00000020: 6566 0203 0765 7870 6563 7473 0203 0463  ef...expects...c
                      ..........
00016980: 0201 0201 0102 f10b f10b 0101 0201 0201  ................
00016990: 1204 850c d90b 01f1 0b01 3f01 0100       ..........?...
```

Now you can use this binary code anywhere without changes,
even under another OS and/or hardware platform, even standalone or with embed olvm.

```bash
# fastrun with ol virtual machine
$ olvm a.bin
arguments: ()
factorial: 6227020800

# try with arguments and print execution result
$ olvm a.bin 42; echo returned: $?
arguments: (42)
factorial: 1405006117752879898543142606244511569936384000000000
returned: 52

# regular ol can run it too
$ ol a.bin 7; echo returned: $?
arguments: (7)
factorial: 5040
returned: 4

# check memory consumption
$ valgrind olvm a.bin 42
==9694== Memcheck, a memory error detector
                      ..........
==9694== HEAP SUMMARY:
==9694==     in use at exit: 0 bytes in 0 blocks
==9694==   total heap usage: 8 allocs, 8 frees, 729,690 bytes allocated
==9694==
==9694== All heap blocks were freed -- no leaks are possible
```

### example 2

You can unwrap command line arguments directly to function arguments.

test.scm:
```scheme
(lambda (what to do . tail)
   (print "arguments: " (cons* what to do tail))

   (print "what: " what)
   (print "to: " to)
   (print "do: " do)
   (print "tail: " tail)

   #true)
```

Let's compile and test:
```bash
$ ol -c --entry test.scm > b.bin

$ ls -la b.bin
-rw-rw-r-- 1 user user 79756 Jan 28 14:10 b.bin

$ ol b.bin 42
error 17 ->
  wrong number of arguments: 1, but #<lambda> expects at least 3

$ ol b.bin "just" "some" "commands" 1 2 3 4 5
arguments: (just some commands 1 2 3 4 5)
what: just
to: some
do: commands
tail: (1 2 3 4 5)
```

(otus async)
============

Ol itself consists of a single purely functional process, but inside it has full support for lightweight coroutines. This enables you to include non-functional features like [dynamic bindings](dynamic-bindings.md), actors, parallel calculations, etc.

Coroutines (REPL is also a coroutine) can communicate asynchronously by sending mails consisting of a sender's name (possibly anonymous) and an arbitrary message in form *#(sender message)*.

Note: there are three similar-looking sending procedures: [mail](#mail), [call](#call), and [bell](#bell).
Feel the difference betheen them: 
* *mail* silently sends a mail, like sending a real *mail*
* *call* sends a mail loudly (waking the recipient if he is sleeping), like *call*ing on the phone
* *bell* just wakes up the recipient (without any mail sending), like using a *bell*.

TOC
---

[async](#async), [actor](#actor), [await](#await),
[async-linked](#async-linked), [actor-linked](#actor-linked), [await-linked](#await-linked),
[sleep](#sleep), [mail](#mail), [wait-mail](#wait-mail), [check-mail](#check-mail),
[wait](#wait)


# async
`(async thunk)`, *procedure*  
`(async name thunk)`, *procedure*

Runs *thunk* (procedure without arguments) asyncronously as a coroutine named *name* if present. Returns the name of the coroutine (even if it's anonymous).

```schem
> (async (lambda ()
     ; a very, very, very long calculations:
     (lfold + 0 (liota 1 1 10000000))))
; continue working independently...
> (* 7 6 9)
```

# actor
`(actor name thunk)`, *procedure*

Runs *thunk* (procedure without arguments) asyncronously as an actor ([wikipedia](https://en.wikipedia.org/wiki/Actor_model), the coroutine with mailbox) named *name*. Returns the name of the actor.

```schem
> (actor 'stack (lambda ()
     (let loop ((this '()))
        (let*((envelope (wait-mail))
              (sender msg envelope))
           (case msg
              (['push val]
                 (loop (cons val this)))
              (['pop]
                 (mail sender (car this))
                 (loop (cdr this))))))))
> (mail 'stack ['push 7])
> (mail 'stack ['push 4])
> (mail 'stack ['push 5])
```

# await
`(await coroutine)`, *procedure*

Stops execution until mail is received from the *coroutine*. Returns a message from received mail.

```schem
; note: example code for `actor` already ran and
;       populated the 'stack actor internal values

> (await (mail 'stack ['pop]))
5

> (await (mail 'stack ['pop]))
4

> (await (mail 'stack ['pop]))
7
```

# async-linked
`(async-linked thunk)`, *procedure*  
`(async-linked name thunk)`, *procedure*

Run *thunk* (a function with no arguments) asyncronously as a coroutine named *name* if present. Coroutine will automatically send the execution result as a mail back to caller.
Returns the name of the coroutine (even if it's anonymous).

```schem
> (define torpid (async-linked (lambda ()
     ; a very, very, very long calculations:
     (lfold + 0 (liota 1 1 10000000)))))
; ohhhhh, let's wait for
> (await-linked torpid)
49999995000000

> (with-exception-handler
     (lambda (x)
        'error-was-thrown)
     (lambda ()
        (define failer (async-linked (lambda ()
           ; ok
           (+ 1 2 3)
           ; error
           ('no 123))))

        (await-linked failer)))
'error-was-thrown
```

# actor-linked
`(actor-linked name thunk)`, *procedure*

Run *thunk* (a function with no arguments) asyncronously as an actor ([wikipedia](https://en.wikipedia.org/wiki/Actor_model), coroutine with a mailbox) named *name*. Coroutine will automatically send the execution result as a mail back to caller.
Returns the name of the actor.

*check the [async-linked](#async-linked) example.*

# await-linked
`(await-linked coroutine)`, *procedure*

Stops execution until mail is received from the *coroutine*. Returns a result of coroutine execution, or throws an error if execution fails.

*check the [async-linked](#async-linked) example.*

# sleep

`(sleep rounds)`, *procedure*

Stops execution for *rounds* number of the current couroutine context switch.
If no other running coroutines are found, a true sleep occurs (as far as the OS is concerned) for 10ms per round. Returns zero.

Note: use [`wait`](wait) to sleep for N milliseconds.

```scheme
> (define (wait-pid pid)
     (let do ()
        (define stat (c/ / (file->string
                              (string-append "/proc/"
                                             (number->string pid)
                                             "/stat"))))
        (unless (string-eq? (third stat) "Z")
           (sleep 10) (do))))
```

# mail
`(mail actor-name message)`, *procedure*

Sends a message to the actor named *actor-name* in the form *#(sender message)*. Message passing (aka mailing) is asynchronous, and at least in a one-core environment order-preserving.

*2.6 version update*:
* returns #false if addressee not found,
* ~~error log "ol: dropping envelope to missing thread" must be enabled explicitly using `(enable-threading-debug!)`, by default no more such logs~~

*check the [actor](#actor) example.*

# wait-mail
`(wait-mail)`, *procedure*

Stops execution until any mail is received. Returns a mail.

*check the [actor](#actor) example.*

# wait-mail (owl io)
`(wait-mail ms)`, *procedure*  
`(wait-mail ms default)`, *procedure*

Stops execution until ms milliseconds have passed or [bell](#bell) or [call](#call) is called.
Returns a mail if any was received, *default* value if no mail and *default* is provided, just #false otherwise.

# check-mail
`(check-mail)`, *procedure*

Returns a mail if it was received, or #false if no mail. Doesn't stop execution.

```scheme
> (actor 'counter (lambda ()
     (let loop ((this 0))
        (define envelope (check-mail))
        (if envelope
        then
           (let*((sender msg envelope))
              (case msg
                 (['reset]
                    (loop 0))
                 (['get]
                    (mail sender this)
                    (loop this))
                 (['stop]
                    #false)
                 (else
                    (runtime-error "unknown command" msg))))
        else
           (sleep 1)
           (loop (+ this 1))))))

> (await (mail 'counter ['get]))
> (mail 'counter ['reset])
> (await (mail 'counter ['get]))
> (mail 'counter ['stop])
```

# bell
`(bell name)`, *procedure*

Wakes the coroutine named *name* if it sleeps. Do nothing otherwise.

# call
`(call name mail)`, *procedure*

Wakes the coroutine named *name* if it sleeps, sends a mail to this coroutine.

# wait-read
`(wait-read port timeout)`, *procedure*

Sleep until port is ready to be read.

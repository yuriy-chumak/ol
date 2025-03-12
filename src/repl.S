    .global _repl, repl

#ifdef __APPLE__
	.section __TEXT,__const
#else
    .section .rodata
#endif

_repl: repl:
    .incbin "repl"

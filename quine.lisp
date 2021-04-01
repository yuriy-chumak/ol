#!/usr/bin/env ol

((lambda (s) (display (list s (list (quote quote) s)))) (quote (lambda (s) (display (list s (list (quote quote) s))))))
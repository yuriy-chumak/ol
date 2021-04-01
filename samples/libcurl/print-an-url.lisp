#!/usr/bin/env ol

(import (lib curl))

(define curl (make-curl))
(curl 'url "https://example.com")
(curl 'perform) ;; or just (curl)

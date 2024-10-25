#!/usr/bin/env ol

(print "It is a standalone executable file that does not require
any external libraries.

Running sandboxed: " *sandbox* "
Command line options: " (command-line) "

Usage: ol [OPTION]... [--] [input-file|. [file-options]]

   --help              print full options list
   -v, --version       print short or long version info
   ...
   --                  end of options list

Use '-' for 'input-file' to read program from the stdin,
use '.' for 'input-file' to run default autorun program.

Note: running 'ol' without any options is same as 'ol .'
")

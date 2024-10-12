#!/usr/bin/env ol

(print "hello :)
Running 'ol' without any options is same as 'ol ./main'.

Usage: ol [OPTION]... [--] [input-file [file-options]]

   --help              print full options list
   -v, --version       print short or long version info
   ...
   --                  end of options list

Use '-' for 'input-file' to read program from the stdin,
use 'main' for 'input-file' to run default main program.
")

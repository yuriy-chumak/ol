#!/usr/bin/env ol

(import
   (only (olvm syscalls) strftime))

(for-each (lambda (arg)
      (define template (car arg))
      (define description (cdr arg))
      (print
         template "\t" description
         (let ((n (string-length description)))
            (if (< n 32)
               (apply string-append (repeat " " (- 32 n)))
               " "))
         "\x1B;[1;32m" (strftime template) "\x1B;[0m"))
   '(
      ("%a" . "Abbreviated weekday name")
      ("%A" . "Full weekday name")
      ("%b" . "Abbreviated month name")
      ("%B" . "Full month name")
      ("%c" . "Date and time representation")
      ("%d" . "Day of the month (01-31)")
      ("%H" . "Hour in 24h format (00-23)")
      ("%l" . "Hour in 12h format (01-12)")
      ("%j" . "Day of the year (001-366)")
      ("%m" . "Month as a number (01-12)")
      ("%M" . "Minute (00-59)")
      ("%p" . "AM or PM designation")
      ("%S" . "Second (00-61)")
      ("%U" . "Week number with the first Sunday as the first day of week one (00-53)")
      ("%w" . "Weekday as a decimal number with Sunday as 0 (0-6)")
      ("%W" . "Week number with the first Monday as the first day of week one (00-53)")
      ("%x" . "Date representation")
      ("%X" . "Time representation")
      ("%y" . "Year, last two digits (00-99)")
      ("%Y" . "Year")
      ("%Z" . "Timezone name or abbreviation")
      ("%%" . "A % sign")
   ))

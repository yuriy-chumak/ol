:: https://en.wikipedia.org/wiki/Regression_testing
erase a.exe
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin
gcc src/olvm.c src/boot.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast
a.exe -e "(let loop ((x 1)) (loop (+ x 1)))"

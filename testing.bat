:: https://en.wikipedia.org/wiki/Regression_testing
erase if exist a.exe
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin
gcc src/olvm.c src/boot.c src/testing.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast
a.exe

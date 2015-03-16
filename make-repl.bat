@echo off
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin

if exist repl.exe  erase repl.exe

:: соберем интерпретатор (с интегрированным образом)
gcc src/olvm.c src/boot.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -O3 -std=c11 -o repl.exe

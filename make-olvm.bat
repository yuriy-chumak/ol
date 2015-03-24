@echo off
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin

if exist olvm.exe  erase olvm.exe

:: соберем интерпретатор (с интегрированным образом)
gcc src/olvm.c -DSTANDALONE -DNOLANGUAGE -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -O3 -std=c11 -o olvm.exe
:: -mpreferred-stack-boundary=4 -fomit-frame-pointer 
if ERRORLEVEL 1 exit
olvm numbers.bin

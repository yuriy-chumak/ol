@echo off
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin

:: соберем интерпретатор (с интегрированным, рабочим образом)
echo Compiling ol virtual machine:
gcc src/olvm.c src/boot.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast
if not ERRORLEVEL 0 exit
echo Ok

:: а теперь выполним компил€цию нового образа
echo.
echo Compiling new ol system image:
a.exe src/ol.scm  2>gc.log
if not ERRORLEVEL 0 exit
echo Ok

:: трасформируем его в C
echo.
echo Preparing new image:
a.exe src/to-c.scm >image 2>>gc.log
echo Ok

:: тестирование полученного образа
echo.
echo Testing new image:
gcc src/olvm.c -x c image src/testing.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o tests.exe
tests.exe 2>>gc.log
if not ERRORLEVEL 0 exit
echo Ok

::recompile
echo Making ol.exe:
gcc src/olvm.c -x c image src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o ol.exe
if not ERRORLEVEL 0 exit
ol.exe -e "(display \"Ok\")"


:: и виртуальную машину заодно
gcc src/olvm.c -DSTANDALONE -DNOLANGUAGE -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -O3 -std=c11 -o vm.exe
vm ok.bin

exit


:: второй образ на основе полученного
ol.exe src/ol.scm
echo Preparing new boot.c...
ol.exe src/to-c.scm >boot2.c
echo Comparing old and new boot files
fc boot.c boot2.c /B >NUL
if errorlevel 1 (
	echo Different
	copy boot2.c boot.c
	goto recompile
) ELSE (
	echo ok
)

echo Making new repl2.exe...
gcc src/olvm.c boot2.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o repl2.exe
repl2.exe -e "(print \"Ok\")"

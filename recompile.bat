@echo off
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin

if exist boot.fasl erase boot.fasl
if exist repl.exe  erase repl.exe
if exist tests.exe erase tests.exe

:: соберем интерпретатор (с интегрированным образом)
gcc src/olvm.c src/boot.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o repl.exe

:: а теперь выполним рекомпиляцию нового образа
repl.exe src/ol.scm 2>gcol1.log
repl.exe src/to-c.scm >boot.c 2>gcol2.log

:: тестирование полученного образа
gcc src/olvm.c boot.c src/testing.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o tests.exe
tests.exe 2>NUL


:: трасформируем его в C
echo Making new repl.exe...
gcc src/olvm.c boot.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o repl2.exe

:: второй образ на основе полученного
echo Preparing new boot.c...
repl2.exe src/ol.scm 2>gcol3.log
repl2.exe src/to-c.scm >boot2.c 2>gcol4.log
echo Comparing old and new boot files
fc boot.c boot2.c /B >NUL
if errorlevel 1 (
	echo Different
) ELSE (
	echo Ok
)

echo Making new repl2.exe...
gcc src/olvm.c boot2.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast -o repl2.exe
repl2.exe -e "(print \"Ok\")"
::repl2.exe tests/numbers.scm

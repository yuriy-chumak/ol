echo off
echo ==========================================================================
echo %%0 = %0
echo %%1 = %1
IF "%1"==""   GOTO ALL
IF "%1"=="vm" GOTO VM
IF "%1"=="boot" GOTO BOOT
IF "%1"=="ol" GOTO OL
IF "%1"=="repl" GOTO REPL
GOTO:EOF

::   repl
:: +------+  boot  +------+  ol  +------+
:: | REPL |------->| BOOT |----->|  OL  |
:: +------+        +------+      +------+
::                    ^              ^
::    vm              |              |
:: +------+           |              |
:: |  VM  |-----------+--------------/
:: +------+
::

:VM
gcc -std=c99 -O2 -Wall -fmessage-length=0 -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32
GOTO:EOF

:BOOT
vm repl <src/to-c.scm >src/boot.c
GOTO:EOF

:OL
gcc -std=c99 -O0 -g3 -Wall -fmessage-length=0 src/boot.c src/olvm.c -o "ol.exe" -lws2_32
GOTO:EOF

:REPL
vm repl < src/ol.scm
FOR %%I IN (repl) DO FOR %%J IN (boot.fasl) DO echo %%~zI -> %%~zJ
fc /b repl boot.fasl > nul
if errorlevel 1 goto again
::  call :BOOT
::  call :OL
	echo '  `___`  '
	echo '  (o,o)  '
	echo '  \)  )  '
	echo '___"_"___'
	echo 'Build Ok.'
	GOTO:EOF
:again
copy boot.fasl repl
GOTO :REPL

:ALL
CALL :VM
CALL :REPL
CALL :BOOT
CALL :OL
GOTO:EOF

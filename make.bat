echo off
echo ==========================================================================
echo %%0 = %0
echo %%1 = %1
IF "%1"==""   GOTO ALL
IF "%1"=="vm" GOTO VM
IF "%1"=="boot" GOTO BOOT
IF "%1"=="ol" GOTO OL
IF "%1"=="repl" GOTO REPL
IF "%1"=="release" GOTO RELEASE
IF "%1"=="/help"  GOTO HELP
IF "%1"=="--help" GOTO HELP
IF "%1"=="/h"     GOTO HELP
IF "%1"=="105"    GOTO 105
GOTO:EOF

:HELP

echo "  repl                                "
echo "+------+  boot  +------+  ol  +------+"
echo "| REPL |------->| BOOT |----->|  OL  |"
echo "+------+        +------+      +------+"
echo "                   ^              ^   "
echo "   vm              |              |   "
echo "+------+           |              |   "
echo "|  VM  |-----------+--------------/   "
echo "+------+                              "
echo.
GOTO:EOF

:VM
gcc -std=c99 -g -Wall -fmessage-length=0 -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32
GOTO:EOF

:BOOT
vm repl <src/to-c.scm >src/boot.c
GOTO:EOF

:OL
gcc -std=c99 -g -Wall -fmessage-length=0 src/boot.c src/olvm.c -o "ol.exe" -lws2_32
GOTO:EOF

:REPL
vm repl < src/ol.scm
FOR %%I IN (repl) DO FOR %%J IN (boot.fasl) DO echo "%%~zI -> %%~zJ"
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

:RELEASE
gcc -std=c99 -O2 -s -Wall -fmessage-length=0 -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32
gcc -std=c99 -O2 -s -Wall -fmessage-length=0 src/boot.c src/olvm.c -o "ol.exe" -lws2_32
GOTO:EOF



:: testing the build in the virtual machines
:105
echo Starting FreeBSD 10.2 x86-64...
"C:\Program Files\Oracle\VirtualBox\VBoxManage" startvm "FreeBSD 10.2 x86-64" --type headless
echo Connecting to the host...
:105_wait
ping 192.168.56.105 -w 1 -n 1 >NUL
if errorlevel 1 goto 105_wait

echo Copying source files...
call :cp 192.168.56.105 Makefile
call :cp 192.168.56.105 src/olvm.c
call :cp 192.168.56.105 src/olvm.h
call :cp 192.168.56.105 src/boot.c
call :cp 192.168.56.105 repl

echo Running make...
plink -ssh -2 -l ol -pw ol -m gmake 192.168.56.105

"C:\Program Files\Oracle\VirtualBox\VBoxManage" controlvm "FreeBSD 10.2 x86-64" savestate

GOTO:EOF

:cp
pscp -l ol -pw ol %~2 %~1:%~2
goto:eof

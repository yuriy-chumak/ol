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
IF "%1"=="tests" GOTO TESTS
IF "%1"=="/help"  GOTO HELP
IF "%1"=="--help" GOTO HELP
IF "%1"=="/h"     GOTO HELP
IF "%1"=="101"    GOTO 101
IF "%1"=="111"    GOTO 111
IF "%1"=="121"    GOTO 121
IF "%1"=="android" GOTO ANDROID
GOTO:EOF


:ALL
CALL :VM
CALL :REPL
CALL :BOOT
CALL :OL
CALL :101
CALL :111
CALL :121
GOTO:EOF

rem 101 - NetBSD x64
rem 111 - FreeBSD x64
rem 121 - OpenBSD x64

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
gcc -std=c99 -g -Wall -fmessage-length=0 -Wno-strict-aliasing -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32 -O2 -s -DNDEBUG -DHAS_PINVOKE=1
GOTO:EOF

:BOOT
vm repl <src/to-c.scm >src/boot.c
GOTO:EOF

:OL
gcc -std=c99 -g -Wall -fmessage-length=0 -Wno-strict-aliasing src/boot.c src/olvm.c -o "ol.exe" -lws2_32 -O2 -s -DNDEBUG -DHAS_PINVOKE=1
GOTO:EOF

:REPL
set VERSION=
for /f "delims=" %%a in ('git describe') do @set VERSION=%%a
vm repl - --version %VERSION% < src/ol.scm
FOR %%I IN (repl) DO FOR %%J IN (boot.fasl) DO echo ":: %%~zI -> %%~zJ"
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

:RELEASE
gcc -std=c99 -O2 -s -Wall -fmessage-length=0 -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32
gcc -std=c99 -O2 -s -Wall -fmessage-length=0 src/boot.c src/olvm.c -o "ol.exe" -lws2_32
GOTO:EOF


:101
CALL :REMOTE 10122 "NetBSD 7.0 x86-64"   gmake
GOTO:EOF

:111
CALL :REMOTE 11122 "FreeBSD 10.2 x86-64" gmake
GOTO:EOF

:121
CALL :REMOTE 12122 "OpenBSD 5.8 x86-64"  gmake
GOTO:EOF

:TEST
echo|set /p=Testing %1 ...
ol %1 >C:\TEMP\out
fc C:\TEMP\out %1.ok > nul
if errorlevel 1 goto fail1
echo Ok.
GOTO:EOF
:fail1
echo Failed.
GOTO:EOF



:TESTS
call :TEST tests\apply.scm
call :TEST tests\banana.scm
call :TEST tests\callcc.scm
call :TEST tests\case-lambda.scm
call :TEST tests\echo.scm
call :TEST tests\ellipsis.scm
call :TEST tests\eval.scm
call :TEST tests\factor-rand.scm
call :TEST tests\factorial.scm
call :TEST tests\fasl.scm
call :TEST tests\ff-call.scm
call :TEST tests\ff-del-rand.scm
call :TEST tests\ff-rand.scm
call :TEST tests\fib-rand.scm
call :TEST tests\hashbang.scm
call :TEST tests\iff-rand.scm
call :TEST tests\library.scm
call :TEST tests\macro-capture.scm
call :TEST tests\macro-lambda.scm
call :TEST tests\mail-order.scm
call :TEST tests\math-rand.scm
call :TEST tests\par-nested.scm
call :TEST tests\par-nested-rand.scm
call :TEST tests\par-rand.scm
call :TEST tests\perm-rand.scm
call :TEST tests\por-prime-rand.scm
call :TEST tests\por-terminate.scm
call :TEST tests\queue-rand.scm
call :TEST tests\record.scm
call :TEST tests\rlist-rand.scm
call :TEST tests\seven.scm
call :TEST tests\share.scm
call :TEST tests\stable-rand.scm
call :TEST tests\str-quote.scm
call :TEST tests\string.scm
call :TEST tests\suffix-rand.scm
call :TEST tests\theorem-rand.scm
call :TEST tests\toplevel-persist.scm
call :TEST tests\utf-8-rand.scm
call :TEST tests\vararg.scm
call :TEST tests\vector-rand.scm
call :TEST tests\numbers.scm
GOTO:EOF

:: ====================================================================================

:ANDROID
D:\Projects\Mobile\android-ndk-r10e\ndk-build.cmd
GOTO:EOF

:REMOTE
echo Starting %~2...
"C:\Program Files\Oracle\VirtualBox\VBoxManage" startvm "%~2" --type headless
:: --type headless

echo Connecting to the host...
:wait
echo.>%TEMP%\empty
plink -ssh -2 -l ol -pw ol 127.0.0.1 -P %~1 -m %TEMP%\empty
if errorlevel 1 goto wait

echo Copying source files...
call :cp 127.0.0.1 %~1 Makefile
call :cp 127.0.0.1 %~1 src/olvm.c
call :cp 127.0.0.1 %~1 src/olvm.h
call :cp 127.0.0.1 %~1 src/boot.c
call :cp 127.0.0.1 %~1 repl

echo Running make...
echo %~3>%TEMP%\gmake
plink -ssh -2 -l ol -pw ol 127.0.0.1 -P %~1 -m %TEMP%\gmake

"C:\Program Files\Oracle\VirtualBox\VBoxManage" controlvm "%~2" savestate
GOTO:EOF


:cp
pscp -l ol -pw ol -P %~2 %~3 %~1:%~3
goto:eof

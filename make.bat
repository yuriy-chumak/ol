@echo off
echo -=( building %1 )=-------------------------------------------------------------------
IF "%1"==""   GOTO ALL
IF "%1"=="vm" GOTO VM
IF "%1"=="vm32" GOTO VM32
IF "%1"=="boot" GOTO BOOT
IF "%1"=="ol" GOTO OL
IF "%1"=="ol32" GOTO OL32
IF "%1"=="repl" GOTO REPL
IF "%1"=="slim" GOTO SLIM
IF "%1"=="talkback" GOTO TALKBACK
IF "%1"=="js" GOTO JS
IF "%1"=="wasm" GOTO WASM
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
for %%a in (
   vm.exe
   src\boot.c
   ol.exe
   src\slim.c
   olvm.js
) do if exist %%a erase %%a
CALL :VM    & if not exist vm.exe goto :fail
CALL :REPL  & fc /b repl boot.fasl > nul & if errorlevel 1 goto :fail
CALL :BOOT  & if not exist src/boot.c goto :fail
CALL :OL    & if not exist ol.exe goto :fail
CALL :TESTS
CALL :SLIM  & if not exist src/slim.c goto :fail
CALL :JS    & if not exist olvm.js goto :fail

echo '  `___`  '
echo '  (o,o)  '
echo '  \)  )  '
echo '___"_"___'
echo 'Build Ok.'
::CALL :101
::CALL :111
::CALL :121
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
echo:
GOTO:EOF

:VM
echo.   *** Making virtual machine:
gcc -std=c99 -g3 -Wall -fmessage-length=0 -Wno-strict-aliasing -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32 -O2 -g2 -DHAS_PINVOKE=1 -m64
GOTO:EOF

:VM32
echo.   *** Making 32-bit virtual machine:
set PATH=C:\mingw\i686-6.2.0-posix-dwarf-rt_v5-rev1\mingw32\bin\;%PATH%
gcc -std=c99 -g0 -Wall -fmessage-length=0 -Wno-strict-aliasing -DNAKED_VM src/olvm.c -o "vm.exe" -lws2_32 -O2 -g2 -DNDEBUG -DHAS_PINVOKE=1 -m32
GOTO:EOF


:BOOT
echo.   *** Making src/boot.c:
vm repl <src/boot.lisp >src/boot.c
GOTO:EOF

:OL
echo.   *** Making Otus Lisp:
gcc -std=c99 -g3 -Wall -fmessage-length=0 -Wno-strict-aliasing src/boot.c src/olvm.c -o "ol.exe" -lws2_32 -O2 -g2 -DHAS_PINVOKE=1 -m64
GOTO:EOF

:OL32
echo.   *** Making 32-bit Otus Lisp:
set PATH=C:\mingw\i686-6.2.0-posix-dwarf-rt_v5-rev1\mingw32\bin\;%PATH%
gcc -std=c99 -g3 -Wall -fmessage-length=0 -Wno-strict-aliasing src/boot.c src/olvm.c -o "ol.exe" -lws2_32 -O2 -g2 -DHAS_PINVOKE=1 -m32
GOTO:EOF

:REPL
set VERSION=1.1
::for /f "delims=" %%a in ('git describe') do @set VERSION=%%a
vm repl - --version %VERSION% < src/ol.scm
FOR %%I IN (repl) DO FOR %%J IN (boot.fasl) DO echo ":: %%~zI -> %%~zJ"
fc /b repl boot.fasl > nul
if errorlevel 1 goto again
GOTO:EOF
:again
copy boot.fasl repl
GOTO :REPL

:SLIM
echo.   *** Making slim:
vm repl src/slim.lisp >src/slim.c
GOTO:EOF

:TALKBACK
echo.   *** Making talkback:
vm repl src/talkback.lisp >src/talkback.repl.c
gcc -std=c99 -g3 -Wall -DEMBEDDED_VM -DNAKED_VM -fmessage-length=0 -Wno-strict-aliasing src/olvm.c src/talkback.repl.c src/talkback.c src/talkback_sample.c -o "oltb.exe" -lws2_32 -O2 -g2 -DHAS_PINVOKE=1
GOTO:EOF

GOTO:EOF

:JS
echo.   *** Making virtual machine on js:
@set PATH=C:\Program Files\Emscripten\python\2.7.5.3_64bit\;C:\Program Files\Emscripten\emscripten\1.35.0\;%PATH%
call emcc src/slim.c src/olvm.c -o olvm.js -s ASYNCIFY=1 -O1 --memory-init-file 0 --llvm-opts "['-O2']"
GOTO:EOF

:WASM
echo.   *** Making virtual machine for webassembly:
@set PATH=C:\Program Files\Emscripten\python\2.7.5.3_64bit\;C:\Program Files\Emscripten\emscripten\1.35.0\;%PATH%
call emcc src/slim.c src/olvm.c -o olvm.html -s ASYNCIFY=1 -s WASM=1 -O1 --memory-init-file 0 --llvm-opts "['-O2']"
GOTO:EOF


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
ol.exe %1 >C:\TEMP\out
fc C:\TEMP\out %1.ok > nul
if errorlevel 1 goto fail1
echo. Ok.
GOTO:EOF
:fail1
echo. Failed.
GOTO:EOF



:TESTS
gcc -std=c99 -g3 -Wall -fmessage-length=0 -Wno-strict-aliasing -DNAKED_VM -DEMBEDDED_VM -DDO_UNIT_TESTING src/olvm.c src/vm-functional-tests.c -o "tests.exe" -lws2_32 -O2 -g2 -DHAS_PINVOKE=1 -m64
tests.exe
if errorlevel 1 goto fail

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
echo:>%TEMP%\empty
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

:fail
echo. *** Build failed!!! ***
exit
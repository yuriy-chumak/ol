@echo off
:: Note: please, use emscripten sdk-1.30.0-64bit,
::       latest sdk is bullshit and doesn't works

set PATH~=%PATH%
set MINGW32=C:\MinGW\i686-7.3.0-posix-dwarf-rt_v5-rev0\mingw32\bin\
set MINGW64=C:\MinGW\x86_64-7.3.0-posix-seh-rt_v5-rev0\mingw64\bin\
set CC=gcc

echo -=( building %1 )=-------------------------------------------------------------------
IF "%1"==""   GOTO ALL
IF "%1"=="vm" GOTO VM
IF "%1"=="vm32" GOTO VM32
IF "%1"=="ol" GOTO OL
IF "%1"=="ol32" GOTO OL32
IF "%1"=="ol64" GOTO OL64
IF "%1"=="repl" GOTO REPL
IF "%1"=="repl32" GOTO REPL32
IF "%1"=="repl64" GOTO REPL64
IF "%1"=="boot" GOTO BOOT
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
IF "%1"=="clean" GOTO CLEAN
GOTO:EOF


:ALL
call :CLEAN
CALL :VM    & if not exist vm.exe goto :fail
CALL :REPL  & fc /b repl boot.fasl > nul & if errorlevel 1 goto :fail
CALL :OL    & if not exist ol.exe goto :fail
CALL :TESTS
CALL :SLIM  & if not exist tmp/slim.c goto :fail
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
echo "+------+                  ol  +------+"
echo "| REPL |--------------------->|  OL  |"
echo "+------+           ^          +------+"
echo "                   |              ^   "
echo "   vm              |              |   "
echo "+------+           |              |   "
echo "|  VM  |-----------+--------------/   "
echo "+------+                              "
echo:
GOTO:EOF

:CLEAN
for %%a in (
   olvm.js
   test-ffi32.exe
   test-ffi64.exe
   test-vm32.exe
   test-vm64.exe
   tmp\repl.c
   tmp\repl.o
   tmp\repl32.o
   tmp\repl64.o
   tmp\slim.c
   vm.exe
   vm32.exe
   vm64.exe
) do if exist %%a erase %%a
GOTO:EOF

:BOOT
vm repl <samples/attic/to-c.scm >src/repl.c
GOTO:EOF

:: ======================================
:VM
echo.   *** Making virtual machine:
set PATH=%MINGW64%;%PATH%
%CC% -std=c99 -g2 -O0 -Wall -fmessage-length=0 -fno-exceptions -Wno-strict-aliasing -DNAKED_VM ^
   src/olvm.c -Iwin32 -o "vm.exe" -lws2_32 -m64
set PATH=%PATH~%
GOTO:EOF

:: ======================================
:VM32
echo.   *** Making 32-bit virtual machine:
set PATH=%MINGW32%;%PATH%

%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -fno-exceptions -Wno-strict-aliasing -DNAKED_VM ^
   src/olvm.c -Iwin32 -o "vm32.exe" -lws2_32 -m32

set PATH=%PATH~%
GOTO:EOF

:: ======================================
:VM64
echo.   *** Making 64-bit virtual machine:
set PATH=%MINGW64%;%PATH%

%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -fno-exceptions -Wno-strict-aliasing -DNAKED_VM ^
   src/olvm.c -Iwin32 -o "vm64.exe" -lws2_32 -m64

set PATH=%PATH~%
GOTO:EOF


:REPL32
set PATH=%MINGW32%;%PATH%
ld -r -b binary -o tmp/repl32.o repl
set PATH=%PATH~%
GOTO:EOF

:REPL64
set PATH=%MINGW64%;%PATH%
ld -r -b binary -o tmp/repl64.o repl
set PATH=%PATH~%
GOTO:EOF


:OL
echo.   *** Making Otus Lisp:
%CC% -std=c99 -g0 -O2 -Wall -fmessage-length=0 -Wno-strict-aliasing tmp/repl.o src/olvm.c -Iwin32 -o "ol.exe" -lws2_32 -DHAS_PINVOKE=1 ^
     -DNDEBUG -s
GOTO:EOF

:OL32
echo.   *** Making 32-bit Otus Lisp:
set PATH=%MINGW32%;%PATH%
%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -Wno-strict-aliasing tmp/repl32.o src/olvm.c -Iwin32 -o "ol.exe" -lws2_32 -DHAS_PINVOKE=1 -m32
set PATH=%PATH~%
GOTO:EOF

:OL64
echo.   *** Making 64-bit Otus Lisp:
set PATH=%MINGW64%;%PATH%
%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -Wno-strict-aliasing tmp/repl64.o src/olvm.c -Iwin32 -o "ol.exe" -lws2_32 -DHAS_PINVOKE=1 -m64
set PATH=%PATH~%
GOTO:EOF

:REPL
set VERSION=1.2
::for /f "delims=" %%a in ('git describe') do @set VERSION=%%a
vm repl - --version %VERSION% < src/ol.scm
FOR %%I IN (repl) DO FOR %%J IN (boot.fasl) DO echo ":: %%~zI -> %%~zJ"
fc /b repl boot.fasl > nul
if errorlevel 1 goto again
ld -r -b binary -o tmp/repl.o repl
GOTO:EOF
:again
copy boot.fasl repl
GOTO :REPL

:SLIM
echo.   *** Making slim:
vm repl src/slim.lisp >tmp/slim.c
GOTO:EOF

:TALKBACK
echo.   *** Making talkback:
ld -r -b binary -o ffi.o otus/ffi.scm
%CC% -std=c99 -g3 -Wall -DEMBEDDED_VM -DNAKED_VM -DOLVM_FFI=1 ^
    -fmessage-length=0 -Wno-strict-aliasing -I src ^
    -D INTEGRATED_FFI ^
    src/olvm.c tmp/repl.o ffi.o extensions/talkback/talkback.c extensions/talkback/sample.c -o "talkback.exe" ^
    -Iwin32 -lws2_32 -O2 -g2
GOTO:EOF

GOTO:EOF

:JS
echo.   *** Making virtual machine on js:
call C:\emscripten\unzip_temp\emsdk_env.bat
if not exist emscripten.js ^
call emcc extensions/asmjsify/asmjsify.c -Iinclude ^
     -o emscripten.js ^
     -s EMTERPRETIFY=1 -s EMTERPRETIFY_ASYNC=1 -s ASSERTIONS=1 ^
     -s SIDE_MODULE=1 ^
     --memory-init-file 0 -O0

call emcc src/olvm.c ^
     -D NAKED_VM=1 -D HAS_DLOPEN=1 ^
     -o olvm.js ^
     -s EMTERPRETIFY=1 -s EMTERPRETIFY_ASYNC=1 ^
     -s MAIN_MODULE=1 ^
     -s EXPORTED_FUNCTIONS="['_main', '_OL_ffi']" ^
     --memory-init-file 0 -Os
GOTO:EOF
::, '_OL_new', '_OL_run', '_OL_free'

:WASM
echo.   *** Making virtual machine for webassembly:
@set PATH=C:\Program Files\Emscripten\python\2.7.5.3_64bit\;C:\Program Files\Emscripten\emscripten\1.35.0\;%PATH%
call emcc tmp/slim.c src/olvm.c -o olvm.html -s ASYNCIFY=1 -s WASM=1 -O1 --memory-init-file 0 --llvm-opts "['-O2']"
GOTO:EOF


:RELEASE
%CC% -std=c99 -O2 -s -Wall -fmessage-length=0 -DNAKED_VM src/olvm.c -Iwin32 -o "vm.exe" -lws2_32 -g0 -DNDEBUG -Wl,-subsystem,windows
%CC% -std=c99 -O2 -s -Wall -fmessage-length=0 tmp/repl.o src/olvm.c -Iwin32 -o "ol.exe" -lws2_32 -g0 -DNDEBUG -Wl,-subsystem,windows
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
vm32.exe repl %1 >C:\TEMP\out
fc C:\TEMP\out %1.ok > nul
if errorlevel 1 goto fail1

vm64.exe repl %1 >C:\TEMP\out
fc C:\TEMP\out %1.ok > nul
if errorlevel 1 goto fail1

echo. Ok.
GOTO:EOF
:fail1
echo. Failed.
GOTO:EOF


:: let's do full package testing (32- and 64-bit binaries)
:TESTS

call :VM32
call :VM64
call :REPL32
call :REPL64

:: internal
set PATH=%MINGW32%;%PATH%
echo 32-bit internal test:
%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -fno-exceptions -Wno-strict-aliasing -DNAKED_VM ^
   src/olvm.c tests/vm.c -Iwin32 -o "test-vm32.exe" -lws2_32 -DEMBEDDED_VM=1 -m32 -DNDEBUG -Iinclude
test-vm32.exe
if errorlevel 1 goto fail

set PATH=%PATH~%
set PATH=%MINGW64%;%PATH%
echo 64-bit internal test:
%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -fno-exceptions -Wno-strict-aliasing -DNAKED_VM ^
   src/olvm.c tests/vm.c -Iwin32 -o "test-vm64.exe" -lws2_32 -DEMBEDDED_VM=1 -m64 -DNDEBUG -Iinclude
test-vm64.exe
if errorlevel 1 goto fail

set PATH=%PATH~%

:: ffi
set PATH=%MINGW32%;%PATH%
echo|set /p=32-bit ffi testing ...
ld -r -b binary -o tmp/repl32.o repl
%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -Wno-strict-aliasing -I src ^
    -DHAS_PINVOKE=1 ^
    src/olvm.c tmp/repl32.o tests/ffi.c -Iwin32 -o "test-ffi32.exe" -lws2_32 -m32
test-ffi32.exe tests/ffi.scm > C:\TEMP\out
fc C:\TEMP\out tests/ffi.scm.ok > nul
if errorlevel 1 (
   echo. Failed.
   goto fail
)
echo. Ok.

set PATH=%PATH~%
set PATH=%MINGW64%;%PATH%
echo|set /p=64-bit ffi testing ...
ld -r -b binary -o tmp/repl64.o repl
%CC% -std=c99 -g3 -O0 -Wall -fmessage-length=0 -Wno-strict-aliasing -I src ^
    -DHAS_PINVOKE=1 ^
    src/olvm.c tmp/repl64.o tests/ffi.c -Iwin32 -o "test-ffi64.exe" -lws2_32 -m64
test-ffi64.exe tests/ffi.scm > C:\TEMP\out
fc C:\TEMP\out tests/ffi.scm.ok > nul
if errorlevel 1 (
   echo. Failed.
   goto fail
)
echo. Ok.

set PATH=%PATH~%

:: Other tests
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
set PATH=%PATH%;C:\android-ndk-r10e\;C:\android-sdk-windows\platform-tools\
call ndk-build
::call adb push libs/armeabi/ol /data/local/tmp/ol
::call adb shell chmod 755 /data/local/tmp/ol
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

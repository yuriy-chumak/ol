::set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin
::gcc src/vm.c src/tests.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32
::make.exe owl
::make.exe o2l

::Debug\vm owl/ol.scm
::Debug\vm src/to-c.scm >src\boot.c

erase if exist a.exe
erase if exist boot.fasl

:: соберем интерпретатор (с интегрированным образом)
set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin
gcc src/olvm.c src/boot.c src/repl.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32 -Ofast

:: а теперь выполним рекомпил€цию нового образа
a.exe src/ol.scm
:: трасформируем его в C
::a.exe src/to-c.scm >boot.c

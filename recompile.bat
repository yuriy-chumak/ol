::set PATH=%PATH%;C:\MinGW\bin;C:\MinGW\msys\1.0\bin
::gcc src/vm.c src/tests.c -IC:\MinGW\include\ -LC:\MinGW\lib\ -lws2_32
::make.exe owl
::make.exe o2l

::Debug\vm owl/ol.scm
Debug\vm src/to-c.scm >src\boot.c

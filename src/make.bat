@set PATH=C:\Program Files\Emscripten\python\2.7.5.3_64bit\;C:\Program Files\Emscripten\emscripten\1.35.0\;%PATH%
::gcc -std=c99 -g3 -Wall -fmessage-length=0 -Wno-strict-aliasing -DNAKED_VM oljs.c -o "js.exe" -O2 -g2 -m64
..\vm ..\repl compile.lisp >program.c
::js.exe program.b <..\tests\numbers.scm
emcc program.c oljs.c -o olvm.html -s ASYNCIFY=1
::emcc program.c oljs.c -o olvm.html -s ASYNCIFY=1 -O2

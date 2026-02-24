requirements:

* javac and keytool - from the `openjdk-8-jdk-headless` package
* android sdk at the `/opt/android/sdk`
* android ndk at the `/opt/android/ndk`
* adb from the `adb` package

build and run:
* `make build`
* `make install` (android device must have developer mode enabled and connected to host)
* `make start`


notes:
the lisp code in "assets" folders is universal and can be run directly on your PC. just cd to "assets" folder and run "main.lisp".

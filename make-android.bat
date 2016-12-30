set PATH=%PATH%;D:\Projects\Mobile\android-ndk-r10e\;D:\Projects\Mobile\android-sdk-windows\platform-tools\
call ndk-build
call adb push libs/armeabi/ol /data/local/tmp/ol
call adb shell chmod 755 /data/local/tmp/ol

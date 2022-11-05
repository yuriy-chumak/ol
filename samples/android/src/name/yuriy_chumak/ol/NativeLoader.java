package name.yuriy_chumak.ol;
public class NativeLoader extends android.app.NativeActivity {
    static {
        System.loadLibrary("olvm");  // Otus Lisp Virtual Machine
        System.loadLibrary("vrapi"); // Virtual Reality API (Oculus)
        System.loadLibrary("gl2es"); // OpenGL 2.1 over GLES 2.0
        System.loadLibrary("main");  // native activity code
    }
}

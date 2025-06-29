package name.yuriy_chumak.ol.nativegl2;

public class Main extends android.app.NativeActivity {
    private static String TAG = "ol";
    static {
        // Otus Lisp
        System.loadLibrary("olvm");  // Ol Virtual Machine
        System.loadLibrary("ol");    // Android JNI + REPL
        // OpenGL
//      System.loadLibrary("vrapi"); // Virtual Reality API (Oculus)
        System.loadLibrary("gl2es"); // OpenGL 2.1 over GLES 2.0
        System.loadLibrary("GLU");   // GLU
        // main()
        System.loadLibrary("main");  // Native Activity code
    }
}

package name.yuriy_chumak.ol.newtondynamics;

public class Main extends android.app.NativeActivity {
    private static String TAG = "ol";
    static {
        // OL
        System.loadLibrary("olvm");  // Ol Virtual Machine
        System.loadLibrary("ol");    // Android JNI + REPL
        // OpenGL
        System.loadLibrary("gl2es"); // OpenGL 2.1 over GLES 2.0
        System.loadLibrary("GLU");   // GLU
        // ND
        System.loadLibrary("newton-dynamics");
        // main()
        System.loadLibrary("main");  // Native Activity code
    }
}

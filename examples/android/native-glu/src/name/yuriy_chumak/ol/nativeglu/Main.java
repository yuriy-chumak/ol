package name.yuriy_chumak.ol.nativeglu;

public class Main extends android.app.NativeActivity {
    private static String TAG = "ol";
    static {
        System.loadLibrary("olvm");  // Ol Virtual Machine
        System.loadLibrary("ol");    // Android JNI + REPL
        System.loadLibrary("gl2es"); // OpenGL over GLES 2.0
        System.loadLibrary("GLU");

        System.loadLibrary("main");  // Native Activity code
    }
}

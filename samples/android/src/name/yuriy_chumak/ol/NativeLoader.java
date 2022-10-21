package name.yuriy_chumak.ol;
public class NativeLoader extends android.app.NativeActivity {
    static {
        System.loadLibrary("olvm");
        System.loadLibrary("vrapi");
        System.loadLibrary("main");
    }
}

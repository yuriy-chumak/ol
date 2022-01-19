package name.yuriy_chumak.ol;

import android.util.Log;
import android.content.res.AssetManager;

public class Olvm
{
	private static String TAG = "ol";

	public static native Object eval(Object... array);
	public static void load(String filename) {
		eval(",load " + "\"" + filename + "\"");
	}

	// new/delete
	public static native void nativeNew();
	public static native void nativeDelete();

	public static native void nativeSetAssetManager(AssetManager am);

	// auto new
	static {
		System.loadLibrary("ol");
		nativeNew();
	}
}
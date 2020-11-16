package name.yuriy_chumak.ol;

import android.util.Log;
import android.content.res.AssetManager;

public class Olvm
{
	private static String TAG = "ol";

	public Olvm(AssetManager assets)
	{
		Log.i(TAG, "Olvm()");
		nativeSetAssetManager(assets);
	}

	public static native Object eval(Object... array);
	public static void load(String filename) {
		Log.i(TAG, "load(" + filename + ")");
		eval(",load " + "\"" + filename + "\"");
	}

	private static native void nativeNew();
	private static native void nativeSetAssetManager(AssetManager am);

	// public static native void nativeDelete();
	static {
		//System.loadLibrary("gl4es");
		// System.loadLibrary("SOIL");
		//System.loadLibrary("freetype2");
		System.loadLibrary("ol");
		nativeNew();
	}
}
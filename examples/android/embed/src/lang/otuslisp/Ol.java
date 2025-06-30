package lang.otuslisp;

import android.util.Log;
import android.content.res.AssetManager;

/**
 * Otus Lisp
 */
public class Ol
{
	// log tag
	private static String TAG = "ol";

	/**
	 * Evaluate Lisp expression
	 * 
	 * Returns evaluated object (string, int, etc.)
	 */
	public static native Object eval(Object... array);

	/**
	 * Load the Lisp file.
	 * 
	 * Will not expose any defines or something,
	 * just load and run.
	 */
	public static void load(String filename) {
		Log.i(TAG, "load \"" + filename + "\"");
		eval(
			"(define-library (" + "!!" + ")" +
			"(import (otus lisp))" +
			"(include \"" + filename + "\")" +
			"(export) (begin #f))"
		);
	}

	// internal new/delete
	public static native void nativeNew();
	public static native void nativeDelete();

	/**
	 * Setup asset manager to be able read package resources.
	 */
	public static native void nativeSetAssetManager(AssetManager am);

	// auto new
	static {
		System.loadLibrary("olvm");// Ol Virtual Machine
		System.loadLibrary("ol");  // Android JNI + REPL

		// create the global Ol instance
		nativeNew();
	}
}

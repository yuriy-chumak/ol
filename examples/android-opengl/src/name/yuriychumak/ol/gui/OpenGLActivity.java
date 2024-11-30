package name.yuriychumak.ol.gui;

import android.app.Activity;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;

// OpenGL support
import android.opengl.GLSurfaceView;
import javax.microedition.khronos.opengles.GL10;
import javax.microedition.khronos.egl.EGLConfig;
import android.opengl.GLES20;

// Ol
import lang.otuslisp.Ol;
import name.yuriychumak.ol.gui.R;

public class OpenGLActivity extends Activity
	implements GLSurfaceView.Renderer
{
	private static String TAG = "ol";
	private GLSurfaceView glView;

	static {
		// we store our libraries in the assets "libraries" folder:
		try {
			android.system.Os.setenv("OL_HOME", "libraries", true);
		} catch (Exception ex) {
			Log.e("ol", ex.toString());
		}

		// load native libraries
		System.loadLibrary("gl2es"); // OpenGL 2.1 over GLES
		System.loadLibrary("GLU"); // OpenGL Utility Library
		System.loadLibrary("SOIL"); // Image loading Library
	}

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		Log.i(TAG, "onCreate()");
		super.onCreate(savedInstanceState);

		requestWindowFeature(android.view.Window.FEATURE_NO_TITLE);
		setContentView(R.layout.main);
		getWindow().addFlags(android.view.WindowManager.LayoutParams.FLAG_FULLSCREEN);

		// ol jni setup (we need to use assets data)
		try {
			Ol.nativeSetAssetManager(this.getAssets());
		} catch (Exception ex) {
			Log.e("ol", ex.toString());
		}

		glView = new GLSurfaceView(this);
		glView.setEGLContextClientVersion(2);
		glView.setRenderer(this);
		// render mode style
		// glView.setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY); /* or RENDERMODE_CONTINUOUSLY */
		glView.setRenderMode(GLSurfaceView.RENDERMODE_CONTINUOUSLY);
		setContentView(glView);
	}


	// GLSurfaceView.Renderer
	public void onSurfaceCreated(GL10 unused, EGLConfig config) {
		Log.d(TAG, "onSurfaceCreated()");

		Ol.load("main.lisp");
		// we needed (gl:force-render)
		Ol.eval("(import (lib gl))");

	}

	public void onSurfaceChanged(GL10 unused, int width, int height) {
		Log.d(TAG, "onSurfaceChanged(" + width + ", " + height + ")");

		// 'resize gl event
		Ol.eval("(mail 'opengl ['resize " + width + " " + height + "])");
	}

	public void onDrawFrame(GL10 unused) {
		Log.v(TAG, "onDrawFrame()");

		// 'render gl event
		Ol.eval("(gl:force-render)");
	}

}

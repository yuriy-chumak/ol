package name.yuriy_chumak.ol;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;

import android.content.Context;
import android.content.res.AssetManager;

import android.opengl.GLSurfaceView;
import javax.microedition.khronos.opengles.GL10;
import javax.microedition.khronos.egl.EGLConfig;
import android.opengl.GLES20;

public class MainActivity extends Activity
    implements GLSurfaceView.Renderer
{
	private static String TAG = "ol";
	private GLSurfaceView glView;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		// setContentView(R.layout.main);

        nativeNew();
        AssetManager assetManager = getApplication().getAssets();
        nativeSetAssetManager(assetManager);

        requestWindowFeature(android.view.Window.FEATURE_NO_TITLE);
        getWindow().addFlags(android.view.WindowManager.LayoutParams.FLAG_FULLSCREEN);

		glView = new GLSurfaceView(this);
        glView.setEGLContextClientVersion(1);
        glView.setRenderer(this);
        //glView.setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);
        glView.setRenderMode(GLSurfaceView.RENDERMODE_CONTINUOUSLY);

		setContentView(glView);

		glView.setOnTouchListener(new OnTouchListener() {
			public boolean onTouch(View view, MotionEvent event) {
				if(event.getAction() == MotionEvent.ACTION_DOWN) {
					final float x = event.getX();
					final float y = event.getY();
                    MainActivity.this.runOnUiThread(new Runnable() {
                        public void run() {
                            onMouseTouch(x, y);
                            glView.requestRender();
                        }
                    });
				}
				return true;
			}
		});
	}

	// GLSurfaceView.Renderer
    public void onSurfaceCreated(GL10 unused, EGLConfig config) {
        load("main.lisp");
    }

    public void onDrawFrame(GL10 unused) {
        eval("(let ((renderer (interact 'opengl ['get 'renderer]))) (if renderer (renderer #false)))");
    }
    public void onMouseTouch(float x, float y)
    {
        //eval("print", "onMouseTouch(", x, " ", y, ")");
        //eval("(let ((mouse-handler (interact 'opengl ['get 'mouse-handler]))) (if mouse-handler (mouse-handler 1 " + x + " " + y + ")))");
    }

    public void onSurfaceChanged(GL10 unused, int width, int height) {
        GLES20.glViewport(0, 0, width, height); // temp
        eval("print", "todo: call viewport with ", width, " ", height);
    }

    public static native void nativeSetAssetManager(AssetManager am);
	public static native void nativeNew();
	// public static native void nativeDelete();

	public static native Object eval(Object... array);
    public static void load(String filename) {
        eval(",load " + "\"" + filename + "\"");
    }

	static {
 		// System.loadLibrary("z");
		// System.loadLibrary("freetype");
		System.loadLibrary("ol");
        System.loadLibrary("SOIL");
	}
}
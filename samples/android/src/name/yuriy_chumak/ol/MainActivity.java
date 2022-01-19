package name.yuriy_chumak.ol;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;

import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;

import android.content.Context;

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
		Log.d(TAG, "onCreate()");

		requestWindowFeature(android.view.Window.FEATURE_NO_TITLE);
		super.onCreate(savedInstanceState);
		// setContentView(R.layout.main);
		getWindow().addFlags(android.view.WindowManager.LayoutParams.FLAG_FULLSCREEN);

		Olvm.nativeSetAssetManager(getApplication().getAssets());
		Olvm.eval("(define *path* (cons \"libraries\" *path*))");

		glView = new GLSurfaceView(this);
		glView.setEGLContextClientVersion(2);
		glView.setRenderer(this);
		glView.setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY); // RENDERMODE_WHEN_DIRTY, RENDERMODE_CONTINUOUSLY

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
		Log.d(TAG, "onSurfaceCreated()");
		Olvm.load("main.lisp");
	}

	public void onDrawFrame(GL10 unused) {
		Log.d(TAG, "onDrawFrame()");
		Olvm.eval("(let ((renderer (await (mail 'opengl ['get 'renderer])))) (if renderer (renderer #false)))");
	}
	public void onMouseTouch(float x, float y)
	{
		Log.d(TAG, "onMouseTouch(" + x + "," + y + ")");

		// eval("(define (p x) (print (inexact x)))");
		// eval("p", x);
		// eval("p", y);
		//eval("(let ((mouse-handler (interact 'opengl ['get 'mouse-handler]))) (if mouse-handler (mouse-handler 1 " + x + " " + y + ")))");
	}

	public void onSurfaceChanged(GL10 unused, int width, int height) {
		Log.d(TAG, "onSurfaceChanged(" + width + ", " + height + ")");

	// 	GLES20.glViewport(0, 0, width, height); // temp
	// 	//eval("print", "todo: call /resize event/ with ", width, " ", height);
	}
}
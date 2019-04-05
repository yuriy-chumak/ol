package name.yuriy_chumak.ol;

import android.app.Activity;
import android.os.Bundle;
import android.widget.Toast;
import android.view.Surface;
import android.view.SurfaceView;
import android.view.SurfaceHolder;
import android.view.View;
import android.view.View.OnClickListener;
import android.util.Log;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageManager.NameNotFoundException;
import android.view.MotionEvent;
import android.view.View.OnTouchListener;
import android.content.res.AssetManager;

import android.content.Context;

import android.opengl.GLSurfaceView;
import javax.microedition.khronos.opengles.GL10;
import javax.microedition.khronos.egl.EGLConfig;
import android.opengl.GLES20;

public class MainActivity extends Activity // implements SurfaceHolder.Callback
{
	private static String TAG = "ol";
    private GLSurfaceView glView;

	class MyGLSurfaceView extends GLSurfaceView {

		private final MyGLRenderer renderer;

		public MyGLSurfaceView(Context context){
			super(context);

			// Create an OpenGL ES 2.0 context
			setEGLContextClientVersion(2);

			renderer = new MyGLRenderer();

			// Set the Renderer for drawing on the GLSurfaceView
			setRenderer(renderer);
			setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);
		}
	}
	public class MyGLRenderer implements GLSurfaceView.Renderer {
		public void onSurfaceCreated(GL10 unused, EGLConfig config) {
			// Set the background frame color
			GLES20.glClearColor(0.0f, 0.0f, 0.0f, 1.0f);

			//eval("(print (glGetString GL_VERSION))");
		}

		public void onDrawFrame(GL10 unused) {
			// Redraw background color
			// GLES20.glClear(GLES20.GL_COLOR_BUFFER_BIT);
			eval("(renderer)");
		}

		public void onSurfaceChanged(GL10 unused, int width, int height) {
			GLES20.glViewport(0, 0, width, height);
		}
	}



	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		glView = new MyGLSurfaceView(this);
		//setContentView(R.layout.main); // glView
		setContentView(glView);

		// // surfaceView.getHolder().addCallback(this);
		//SurfaceView surfaceView = (SurfaceView)findViewById(R.id.surfaceview);
		glView.setOnTouchListener(new OnTouchListener() {
			public boolean onTouch(View view, MotionEvent event) {
				if(event.getAction() == MotionEvent.ACTION_DOWN) {
					float x = event.getX();
					float y = event.getY();
					nativePostEvent(1, (int)x, (int)y);
				}
				return true;
			}
		});

		ApplicationInfo appInfo = null;
		PackageManager packMgmr = this.getPackageManager();
		try {
			appInfo = packMgmr.getApplicationInfo("name.yuriy_chumak.ol", 0);
		} catch (NameNotFoundException e) {
			e.printStackTrace();
			throw new RuntimeException("Unable to locate APK...");
		}

		String apkLocation = appInfo.sourceDir;
        AssetManager assetManager = getApplication().getAssets();

		nativeSetApkLocation(apkLocation);
		nativeSetOlHome("/sdcard/WnD");
        nativeSetAssetManager(assetManager);
	}


	@Override
	protected void onStart() {
		super.onStart();
		nativeOnStart();

        load("reference.scm");
	}

	@Override
	protected void onResume() {
		super.onResume();
		nativeOnResume();
	}

	@Override
	protected void onPause() {
		super.onPause();
		nativeOnPause();
	}

	@Override
	protected void onStop() {
		super.onStop();
		nativeOnStop();
	}

	public void surfaceChanged(SurfaceHolder holder, int format, int w, int h) {
		//nativeSetSurface(holder.getSurface());
	}
	public void surfaceCreated(SurfaceHolder holder) {
	}

	public void surfaceDestroyed(SurfaceHolder holder) {
		//nativeSetSurface(null);
	}

	public static native void nativeOnStart();
	public static native void nativeOnResume();
	public static native void nativeOnPause();
	public static native void nativeOnStop();

	public static native void nativeSetApkLocation(String location);
	public static native void nativeSetOlHome(String home);
    public static native void nativeSetAssetManager(AssetManager am);
	public static native void nativePostEvent(int button, int x, int y);

	public static native Object eval(Object... array);
    public static void load(String filename) {
        eval(",load " + "\"" + filename + "\"");
    }

	static {
		System.loadLibrary("z");
		System.loadLibrary("freetype");
		System.loadLibrary("ol");
	}
}
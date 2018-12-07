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

public class MainActivity extends Activity implements SurfaceHolder.Callback
{
	private static String TAG = "Otus Lisp";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.main);

		SurfaceView surfaceView = (SurfaceView)findViewById(R.id.surfaceview);
		surfaceView.getHolder().addCallback(this);
		surfaceView.setOnClickListener(new OnClickListener() {
				public void onClick(View view) {
					MainActivity.this.finish();
					System.exit(0);
					// Toast toast = Toast.makeText(MainActivity.this,
					//							  "This demo combines Java UI and native EGL + OpenGL renderer",
					//							  Toast.LENGTH_LONG);
					// toast.show();
				}});

		String apkLocation;
		ApplicationInfo appInfo = null;
		PackageManager packMgmr = this.getPackageManager();
		try {
			appInfo = packMgmr.getApplicationInfo("name.yuriy_chumak.ol", 0);
		} catch (NameNotFoundException e) {
			e.printStackTrace();
			throw new RuntimeException("Unable to locate APK...");
		}

		apkLocation = appInfo.sourceDir;
		nativeSetApkLocation(apkLocation);

		nativeSetOlHome("/sdcard");
	}

	@Override
	protected void onStart() {
		super.onStart();
		nativeOnStart();
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
		nativeSetSurface(holder.getSurface());
	}
	public void surfaceCreated(SurfaceHolder holder) {
	}

	public void surfaceDestroyed(SurfaceHolder holder) {
		nativeSetSurface(null);
	}

	public static native void nativeOnStart();
	public static native void nativeOnResume();
	public static native void nativeOnPause();
	public static native void nativeOnStop();

	public static native void nativeSetSurface(Surface surface);
	public static native void nativeSetApkLocation(String location);
	public static native void nativeSetOlHome(String home);

	static {
		System.loadLibrary("freetype");
		System.loadLibrary("ol");
	}
}
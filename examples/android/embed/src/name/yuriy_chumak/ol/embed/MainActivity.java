package name.yuriy_chumak.ol.embed;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.widget.TextView;

import lang.otuslisp.Ol;

import name.yuriy_chumak.ol.embed.R;

public class MainActivity extends Activity {
	boolean running = true;
	static String numbers = "";

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main);

		TextView notepad = findViewById(R.id.notepad);

		try {
			Ol.nativeSetAssetManager(this.getAssets());
		} catch (Exception ex) {
			Log.e("ol", ex.toString());
		}

		try {
			Ol.eval("(import (pi))");
		}
		catch (Exception ex) {
			Log.e("ol", ex.toString());
		}

		running = true;
		new Thread() {
			@Override
			public void run()
			{
				while (running)
				try {
					if (numbers.length() == 1)
						numbers += ".";
					numbers += Ol.eval("(pi)");
					runOnUiThread(new Runnable() {
						@Override
						public void run() {
							notepad.setText(numbers);
						}
					});

					Thread.sleep(1000);
				}
				catch (Exception ex) {
					Log.e("ol", ex.toString());
				}
			}
		}.start();
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();
		Log.i("ol", "onDestroy()");

		running = false;
	}
}

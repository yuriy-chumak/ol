package name.yuriy_chumak.ol.tui;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.widget.TextView;

import lang.otuslisp.Ol;

import name.yuriy_chumak.ol.tui.R;

public class MainActivity extends Activity {
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

		new Thread() {
			@Override
			public void run()
			{
				while (true)
				try {
					if (notepad.getText().length() > 2 && notepad.getText().charAt(notepad.getText().length() - 2) == '\n')
						notepad.append(".");
					notepad.append(Ol.eval("(pi)").toString());

					Thread.sleep(1000);
				}
				catch (Exception ex) {
					Log.e("ol", ex.toString());
				}
			}
		}.start();	// run the thread
	}

	@Override
	protected void onStart() {
		super.onStart();
		
		Log.i("ol", "onStart()");
	}	

	@Override
	protected void onDestroy() {
		super.onDestroy();

		Log.i("ol", "onDestroy()");
	}
}

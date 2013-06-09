package com.example.myfirstapp;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.content.Intent;
import android.widget.EditText;

public class MainActivity extends Activity
{
    public final static String EXTRA_MESSAGE = 'com.example.myfirstapp.MESSAGE'; // Good practice to use app package-name as prefix.  Avoids collisions with other packages/apps.
    
    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
    }
    public void sendMessage (View view){
	Intent intent = new Intent(this, DisplaceMessageActivity.class);
	EditText editText = (EditText) findViewById(R.id.edit_message);
	String message = editText.getText().toString();
	intent.putExtra(EXTRA_MESSAGE, message);
	startActivity(intent);
    }
}

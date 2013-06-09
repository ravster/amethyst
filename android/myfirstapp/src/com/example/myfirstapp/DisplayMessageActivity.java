public class DisplayMessageActivity extends Activity {

    @SuppressLint("NewApi")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_display_message);

        // Make sure we're running on Honeycomb or higher to use ActionBar APIs
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB) {
            // Show the Up button in the action bar.
            getActionBar().setDisplayHomeAsUpEnabled(true);
        }

	// Get the message from the intent that began this Activity.
	Intent intent = getIntent();
	String message = intent.getStringExtra(MainActivity.EXTRA_MESSAGE);

	// Create the text view to display the message
	TextView textView = new TextView(this); // Why 'this'?
	textView.setTextSize(40);
	textView.setText(message);

	// Set the text view as the layout.
	// Not put the text view in the layout?
	setContentView(textView);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
        case android.R.id.home:
            NavUtils.navigateUpFromSameTask(this);
            return true;
        }
        return super.onOptionsItemSelected(item);
    }
}

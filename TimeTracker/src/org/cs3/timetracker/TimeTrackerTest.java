
package org.cs3.timetracker;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Timer;

import junit.framework.TestCase;

/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author linder
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TimeTrackerTest extends TestCase {
	
	TestTimeObserver observer;
	TimeTicker ticker;

	public class TestTimeObserver implements ITimeObserver {		
		public String Log = "";
		
		public TestTimeObserver() {
			Log = Log + "TestTimeObserver started.\n";
		}
		
		public void notify(TimeEvent time) {
			Log = Log + "Time message received. Minutes = "+time.getMinutes()+"; Seconds = "+time.getSeconds();
		}
		
	}
	
	/*
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();

		observer = new TestTimeObserver();
		ticker = new TimeTicker();
		
		ticker.addObserver(observer);
		ticker.start();
		
		Thread sleeper = new Thread();
		try 
		{
			Thread.sleep(5000);
		} catch(Exception e) { e.printStackTrace(); }
	}
	
	public void testThisUnit() throws Exception 
	{
		String ObserverTestString = 
				"TestTimeObserver started.\n" 
			+	"Time message received. Minutes = 3; Seconds = 0" 	
			+	"Time message received. Minutes = 2; Seconds = 59" 	
			+	"Time message received. Minutes = 2; Seconds = 58" 	
			+	"Time message received. Minutes = 2; Seconds = 57" 	
			+	"Time message received. Minutes = 2; Seconds = 56";
		
		String TimeTickerTestString = 
				"Started.\n";
		
		assertEquals("Observer Test missed.", ObserverTestString, observer.Log);
		assertEquals("Time Ticker Test missed.", TimeTickerTestString, ticker.Log);
	}
	

	/**
	 * Constructor for TimeTrackerTest.
	 * @param name
	 */
	public TimeTrackerTest(String name) {
		super(name);
	}

}


package org.cs3.timetracker;

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
			Log = Log + "Time message received. Minutes = "+time.getMinutes()+"; Seconds = "+time.getSeconds()+"\n";
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
		
		System.out.println("STARTING the TimeTicker now and will wait 5 seconds.");
		Thread sleeper = new Thread();
		try 
		{
			Thread.sleep(4800);
		} catch(Exception e) { e.printStackTrace(); }
		
		System.out.println("PAUSING the Ticker now, waiting 2 seconds.");
		ticker.pause();
		
		try
		{
			Thread.sleep(1800);
		} catch(Exception e) { e.printStackTrace(); }
		
		System.out.println("RESUMING the Ticker for 4 seconds.");
		ticker.resume();
		
		try 
		{
			Thread.sleep(3800);
		} catch(Exception e) { e.printStackTrace(); }		
	
		System.out.println("STOPPING the Ticker now.");
		ticker.stop();
		
		try
		{
			Thread.sleep(500);
		} catch(Exception e) { e.printStackTrace(); }
	}
	
	public void testObserver() throws Exception 
	{
		String ObserverTestString = 
				"TestTimeObserver started.\n" 
			+	"Time message received. Minutes = 3; Seconds = 0\n" 	
			+	"Time message received. Minutes = 2; Seconds = 59\n" 	
			+	"Time message received. Minutes = 2; Seconds = 58\n" 	
			+	"Time message received. Minutes = 2; Seconds = 57\n" 	
			+	"Time message received. Minutes = 2; Seconds = 56\n"
			+	"Time message received. Minutes = 2; Seconds = 55\n"
			+	"Time message received. Minutes = 2; Seconds = 54\n"
			+	"Time message received. Minutes = 2; Seconds = 53\n"
			+	"Time message received. Minutes = 2; Seconds = 52\n";
			
		assertEquals("Observer Test missed.", ObserverTestString, observer.Log);
	}
	
	public void testTicker() throws Exception
	{
		String TimeTickerTestString = 
				"Started.\n"
			+	"Paused.\n"
			+ 	"Resumed.\n"
			+	"Stopped.\n";
	
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

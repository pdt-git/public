
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

	public class TestTimeObserver implements ITimeObserver {
		
		public TestTimeObserver() {
			System.out.println("I have been started.");
		}
		
		public void notify(TimeEvent time) {
			System.out.println("Received time Message. Minutes = "+ time.getMinutes()+" ; Seconds = "+time.getSeconds());
		}
		
	}
	
	/*
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();

		TimeTicker ticker = new TimeTicker();
		TestTimeObserver observer = new TestTimeObserver();
		
		ticker.addObserver(observer);
		ticker.start();
		
		Thread sleeper = new Thread();
		try {
		Thread.sleep(5000);
		}catch(Exception e){ e.printStackTrace();}
		
//		Timer testTimer = new Timer(5000, new ActionListener() {
//			public void actionPerformed(ActionEvent event) {
//				ticker.stop();
//			}
//		});
//		
//		testTimer.start();
	}
	
	public void testThisUnit() throws Exception 
	{
		assertEquals(2,2);	
	}
	

	/**
	 * Constructor for TimeTrackerTest.
	 * @param name
	 */
	public TimeTrackerTest(String name) {
		super(name);
	}

}

/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker;

import junit.framework.TestCase;

/**
 * @author thiesa
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TimeEventTest extends TestCase {

	public void testGetFormattedString() {
		
		TimeEvent myTimeEvent = new TimeEvent(8,6);
		assertEquals("Test for correct format, minutes and seconds < 10", "08:06", myTimeEvent.getFormattedString());
		
		myTimeEvent = new TimeEvent(10,22);
		assertEquals("Test for correct format, minutes and seconds > 10", "10:22", myTimeEvent.getFormattedString());

		
	}

}

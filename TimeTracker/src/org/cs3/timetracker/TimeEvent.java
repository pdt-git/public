package org.cs3.timetracker;
/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */

/**
 * @author speicher
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TimeEvent {
	private int minutes; // 0..59
	private int seconds; // 0..59
	
	public TimeEvent(int min, int sec) {
		minutes = min;
		seconds = sec;
	}
	
	public int getMinutes() {
		return minutes;
	}
	
	public int getSeconds() {
		return seconds;
	}

	/**
	 * @return
	 */
	public String getFormattedString() {
		String outString ="";
		outString = normalizeTime(outString, minutes);
		outString += ":";
		outString = normalizeTime(outString, seconds);
		return outString;
	}


	private String normalizeTime(String outString, int time) {
		if (time<10) outString = outString + "0";
		outString += time;
		return outString;
	}
	
	public void setMinutes(int Min)
	{
		minutes = Min;
	}
	
	public void setSeconds(int Sec)
	{
		seconds = Sec;
	}
}

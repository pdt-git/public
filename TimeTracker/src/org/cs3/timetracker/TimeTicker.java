package org.cs3.timetracker;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;

import javax.swing.Timer;

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
public class TimeTicker  {
	private Timer t;
	private TimeEvent time;
	private ArrayList observers;
	
	
	public String Log = ""; 
	
	private int seconds;

	private int lastTimeStamp;
	private static final int INITSECONDS = 180;
	

	private void init(){
		seconds = INITSECONDS;
		lastTimeStamp = seconds;
	}
	
	
	public int getTimeDifference(){
		return Math.abs(seconds - lastTimeStamp);
	}
	
	
	public void resetTimeStamp(){
		lastTimeStamp = seconds;
	}
	
	public void addObserver(ITimeObserver observer)
	{
		observers.add(observer);	
	}
	
	public void timeTick()
	{
		time.setMinutes(seconds / 60);
  		time.setSeconds(seconds - ((seconds / 60) * 60));
  		seconds--;

  		Iterator i = observers.iterator();
  		while (i.hasNext()) {
  			ITimeObserver a = (ITimeObserver) i.next();
  			a.notify(time);
  		}
  		
  		if (seconds == -1) {
  			stop();
  		}
	}

	public TimeTicker()
	{
		init();
		observers = new ArrayList();
		time = new TimeEvent(0,0);
		ActionListener action = new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				timeTick();
			}
		};
				
		t = new Timer(0, action);
		t.setRepeats(false);
	}
	
	public void start()
	{
		init();
		
		t.setRepeats(true);
		t.setDelay(1000);
		t.start();
		
		Log = Log + "Started.\n";
	}
	
	public void stop()
	{
		t.setRepeats(false);
		t.stop();
		seconds = 0;
		Log = Log + "Stopped.\n";
	}
	
	public void pause()
	{
		t.stop();
		Log = Log + "Paused.\n";
	}
	
	public void resume()
	{
		t.start();
		Log = Log + "Resumed.\n";
	}

	/**
	 * @return Returns the lastTimeStamp.
	 */
	public int getLastTimeStamp() {
		return lastTimeStamp;
	}
	/**
	 * @return Returns the seconds.
	 */
	public int getCurrentSeconds() {
		return seconds;
	}
}

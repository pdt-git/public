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
	
	private int Seconds = 180; // 3* 60
	
	public void addObserver(ITimeObserver observer)
	{
		observers.add(observer);	
	}
	
	public void TimeTick()
	{
		time.setMinutes(Seconds / 60);
  		time.setSeconds(Seconds - ((Seconds / 60) * 60));

  		Iterator i = observers.iterator();
  		while (i.hasNext()) {
  			ITimeObserver a = (ITimeObserver) i.next();
  			a.notify(time);
  		}
  		  		
  		Seconds--;	
	}

	public TimeTicker()
	{
		observers = new ArrayList();
		time = new TimeEvent(0,0);
		ActionListener action = new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				TimeTick();
			}
		};
				
		t = new Timer(0, action);
		t.setRepeats(true);
	}
	
	public void start()
	{
		Seconds = 180;
		t.setDelay(1000);
		t.start();
		
		Log = Log + "Started.\n";
	}
	
	public void stop()
	{
		t.stop();
		Seconds = 0;
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

}

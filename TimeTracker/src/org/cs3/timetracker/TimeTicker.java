package org.cs3.timetracker;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

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

	public TimeTicker()
	{
	
	}
	
	public void start()
	{
      Timer t;
      
      t = new Timer(30, new ActionListener() {
      	public void actionPerformed(ActionEvent _)
      	{
      		// 
      	}
      });
	}
	
	public void stop()
	{
		
	}
	
	public void pause()
	{
	
	}
	
	public void resume()
	{
		
	}

}

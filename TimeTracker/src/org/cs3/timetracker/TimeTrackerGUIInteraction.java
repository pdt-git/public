/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker;

import java.awt.FlowLayout;

import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;

/**
 * @author schmitzs
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TimeTrackerGUIInteraction implements ITimeObserver, MouseListener{
	private Composite buttonpanel;
	private Button startbutton;
	private Button pausebutton;
	private Button continuebutton;
	private Button stopbutton;
	private TimeTracker timetracker;
	
	public void notify(TimeEvent time){
		if(time.getMinutes()==0 && time.getSeconds()==0){
			startbutton.setEnabled(true);
			stopbutton.setEnabled(false);
			pausebutton.setEnabled(false);
			continuebutton.setEnabled(false);
		}
	}
	
	public TimeTrackerGUIInteraction(Composite parent){
		buttonpanel = new Composite(parent, 1);	//TODO find ot static number
		startbutton = new Button(buttonpanel, 1);
		startbutton.setText("start");
		startbutton.addMouseListener(this);
		pausebutton = new Button(buttonpanel, 1);
		pausebutton.setText("pause");
		pausebutton.addMouseListener(this);
		continuebutton = new Button(buttonpanel, 1);
		continuebutton.setText("continue");
		continuebutton.addMouseListener(this);
		stopbutton = new Button(buttonpanel, 1);
		stopbutton.setText("stop");
		stopbutton.addMouseListener(this);
	}
	
	public void addTimeTracker(TimeTracker tracker){
		timetracker = tracker;
	}
	
	public void mouseDown(MouseEvent e){
		switch(e.button){
		case 1:	timetracker.start();
				startbutton.setEnabled(false);
				stopbutton.setEnabled(true);
				pausebutton.setEnabled(true);
				break;
		case 2: timetracker.pause();
				continuebutton.setEnabled(true);
				pausebutton.setEnabled(false);
				break;
		case 3: timetracker.resume();
				pausebutton.setEnabled(true);
				continuebutton.setEnabled(false);
				break;
		case 4: timetracker.stop();
				startbutton.setEnabled(true);
				stopbutton.setEnabled(false);
				pausebutton.setEnabled(false);
				continuebutton.setEnabled(false);
				break;
		}
	}
	
	public void mouseUp(MouseEvent e){
		
	}
	
	public void mouseDoubleClick(MouseEvent e){
		
	}
	
}

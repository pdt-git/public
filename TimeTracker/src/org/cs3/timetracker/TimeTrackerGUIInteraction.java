/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker;


import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.widgets.*;

/**
 * @author schmitzs
 *
 * Module: GUI Interaction. Contains 4 buttons - Start, Pause, Continue, Stop.
 * Should have a reference of TimeTracker to call appropriate methods when 
 * clicked on the respective button.
 * 
 */
public class TimeTrackerGUIInteraction implements ITimeObserver, MouseListener{

/////////////////////////////////////////////////////////////////////
//// P R I V A T E   M E M B E R   V A R I A B L E S
	private Button startbutton;
	private Button pausebutton;
	private Button continuebutton;
	private Button stopbutton;
	private TimeTicker timetracker;
	private Composite composite;
	private Logger log;
	

/////////////////////////////////////////////////////////////////////
////IMPLEMENTATION METHODS
	
	/**
	 * Implementation method of ITimeObserver
	 */
	public void notify(TimeEvent time){
		System.out.println("Min: " + time.getMinutes() + " Sec: " + time.getSeconds());
		if(!TimeTrackerPlugin.getDefault().isCountingUp() && time.getMinutes()==0 && time.getSeconds()==0){
			if(!composite.isDisposed())

			TimeTrackerPlugin.getDefault().getWorkbench().getDisplay().asyncExec(
					new Runnable(){
						public void run() {
							startbutton.setEnabled(true);
							stopbutton.setEnabled(false);
							pausebutton.setEnabled(false);
							continuebutton.setEnabled(false);
						}
					});			
		}
	}
	

	/**
	 * Implementation method of MouseListener
	 */
	public void mouseDown(MouseEvent e){
		Button temp = (Button) e.getSource();
		String match = temp.getText();
		if(match.equalsIgnoreCase("start")){
			timetracker.start();
			startbutton.setEnabled(false);
			continuebutton.setEnabled(false);
			stopbutton.setEnabled(true);
			pausebutton.setEnabled(true);
		}
		if(match.equalsIgnoreCase("pause")){
			timetracker.pause();
			startbutton.setEnabled(false);
			continuebutton.setEnabled(true);
			pausebutton.setEnabled(false);
			stopbutton.setEnabled(true);
			
		}
		if(match.equalsIgnoreCase("continue")){
			timetracker.resume();
			startbutton.setEnabled(false);
			pausebutton.setEnabled(true);
			continuebutton.setEnabled(false);
			stopbutton.setEnabled(true);
		}
		if(match.equalsIgnoreCase("stop")){
			timetracker.stop();
			startbutton.setEnabled(true);
			stopbutton.setEnabled(false);
			pausebutton.setEnabled(false);
			continuebutton.setEnabled(false);
		}
		
	}
	
	
	/**
	 * Implementation method of MouseListener
	 */
	public void mouseUp(MouseEvent e){
		
	}
	
	/**
	 * Implementation method of MouseListener
	 */
	public void mouseDoubleClick(MouseEvent e){
		
	}
	
	
/////////////////////////////////////////////////////////////////////
//// PUBLIC Methods
	
	/**
	 * Constructor method
	 * @param parent
	 * 
	 * Initiates the button and add them to parent (Composite)
	 */
	public TimeTrackerGUIInteraction(Composite parent, Logger logger){
		composite = parent;
		log= logger;
		startbutton = new Button(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		startbutton.setText("start");
		startbutton.addMouseListener(this);
		pausebutton = new Button(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		pausebutton.setText("pause");
		pausebutton.addMouseListener(this);
		continuebutton = new Button(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		continuebutton.setText("continue");
		continuebutton.addMouseListener(this);
		stopbutton = new Button(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		stopbutton.setText("stop");
		stopbutton.addMouseListener(this);
		startbutton.setEnabled(true);
		stopbutton.setEnabled(false);
		pausebutton.setEnabled(false);
		continuebutton.setEnabled(false);
	}
	
	/**
	 * Add TimeTracker object to call the methods
	 * @param tracker
	 */
	public void addTimeTracker(TimeTicker tracker){
		timetracker = tracker;
	}
	
}

/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker.views;

import junit.framework.TestCase;

import org.cs3.timetracker.TimeEvent;
import org.cs3.timetracker.TimeTrackerPlugin;
import org.eclipse.ui.PartInitException;

/**
 * @author thiesa
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class TimeTrackerGUIViewerTest extends TestCase {

	/**
	 * @throws PartInitException
	 * 
	 */
	public void testInitialization() throws PartInitException {
//		Workbench.getInstance().
		TimeTrackerGUIViewer viewer = (TimeTrackerGUIViewer)TimeTrackerPlugin.getDefault()
				.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.showView("org.cs3.timetracker.TimeTrackerGUIViewer");
		
		TimeEvent myTimeEvent = new TimeEvent(3,5);
		viewer.notify(myTimeEvent);
		try {
			Thread.sleep(5000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}

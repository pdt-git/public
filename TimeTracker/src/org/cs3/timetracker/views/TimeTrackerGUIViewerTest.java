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
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Event;
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
		assertEquals("03:05",viewer.getViewer().getElementAt(0).toString());
		
		assertEquals(true,viewer.getComposite().getChildren()[0].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[1].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[2].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[3].isEnabled());
		
		clickButton(viewer,0);
		assertEquals(false,viewer.getComposite().getChildren()[0].isEnabled());
		assertEquals(true,viewer.getComposite().getChildren()[1].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[2].isEnabled());
		assertEquals(true,viewer.getComposite().getChildren()[3].isEnabled());
		
		clickButton(viewer,1);
		assertEquals(false,viewer.getComposite().getChildren()[0].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[1].isEnabled());
		assertEquals(true,viewer.getComposite().getChildren()[2].isEnabled());
		assertEquals(true,viewer.getComposite().getChildren()[3].isEnabled());
		
		clickButton(viewer,2);
		assertEquals(false,viewer.getComposite().getChildren()[0].isEnabled());
		assertEquals(true,viewer.getComposite().getChildren()[1].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[2].isEnabled());
		assertEquals(true,viewer.getComposite().getChildren()[3].isEnabled());
		
		clickButton(viewer,3);
		assertEquals(true,viewer.getComposite().getChildren()[0].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[1].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[2].isEnabled());
		assertEquals(false,viewer.getComposite().getChildren()[3].isEnabled());
	}

	/**
	 * @param viewer
	 * @param event
	 */
	private void clickButton(TimeTrackerGUIViewer viewer,int num) {
		Event event = new Event();
		MouseEvent mouseEvent;
		event.widget = (Button)viewer.getComposite().getChildren()[num];
		mouseEvent = new MouseEvent(event);
		viewer.getGuiInteraction().mouseDown(mouseEvent);
	}
}

/*
 * Created on 30.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package TimeTracker.views;

import org.cs3.timetracker.TimeTrackerPlugin;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.internal.Workbench;

import junit.framework.TestCase;

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
	private void testInitialization() throws PartInitException {
//		Workbench.getInstance().
		TimeTrackerGUIViewer viewer = (TimeTrackerGUIViewer)TimeTrackerPlugin.getDefault()
				.getWorkbench().getActiveWorkbenchWindow().getActivePage()
				.showView("org.cs3.timetracker.TimeTrackerGUIViewer");
		
		
		
	}
}

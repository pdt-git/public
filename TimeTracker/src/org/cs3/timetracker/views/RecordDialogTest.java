/*
 * Created on 01.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker.views;

import junit.framework.TestCase;

import org.cs3.timetracker.TimeTrackerPlugin;
import org.eclipse.swt.widgets.Shell;

/**
 * @author linder
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class RecordDialogTest extends TestCase {

	RecordDialog Dialog;
	
	/*
	 * @see TestCase#setUp()
	 */
	protected void setUp() throws Exception {
		super.setUp();
			
		Shell myShell = TimeTrackerPlugin.
		getDefault().getWorkbench().
		getActiveWorkbenchWindow().getShell(); 
				
		Dialog = new RecordDialog(myShell);
		Dialog.open();
	}
	
	public void testThis() throws Exception
	{
		System.out.println("Result = "+Dialog.getReturnCode());
		System.out.println("Comment = "+Dialog.getValue());
		
		assertEquals(1,1);
	}

}

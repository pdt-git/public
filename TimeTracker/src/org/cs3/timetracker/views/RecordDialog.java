/*
 * Created on 01.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.timetracker.views;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * @author linder
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class RecordDialog extends InputDialog {
	
	/**
	 * @param parentShell
	 * @param dialogTitle
	 * @param dialogMessage
	 * @param initialValue
	 * @param validator
	 */
	public RecordDialog(Shell parentShell) {
		super(parentShell,
				// Dialog's title
				"Recording...",
				// Dialog's message
				"Please enter a comment and press \"Record\" to "
			+	"save this pause to the log, otherwise press "
			+	"\"Cancel\" to continue...",
				
				"", null);
	}
	
	/*
	 * open() gives back
	 *   0 if "Record" is pressed. one has to check, if
	 *   a valid comment has been entered.
	 * 
	 *   1 if "Cancel" is pressed. CAUTION! The comment
	 *   string is "null" !!
	 * 
	 */
	public int open()
	{
		setBlockOnOpen(true);
		int i = super.open();
		
		return i; 
	}
	
	protected void createButtonsForButtonBar(Composite parent) 
	{
		super.createButtonsForButtonBar(parent);
		
		getButton(IDialogConstants.OK_ID).setText("Record");
		// The opinion was to enable this button just
		// when something is entered in the comment line.
		// But it is impossible to check this from outside.
		// getButton(IDialogConstants.OK_ID).setEnabled(false);

		// getButton(IDialogConstants.CANCEL_ID).setText("Cancel");
	}

}

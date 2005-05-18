package org.cs3.pdt.internal.actions;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.UIUtils;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.internal.Workbench;

public class HelpActionDelegate extends QueryActionDelegate {

	public HelpActionDelegate() {
		super("help", "opening prolog help");
//		
//		UIUtils.displayMessageDialog(PDTPlugin.getDefault().getWorkbench().getActiveWorkbenchWindow().getShell(),"PDT",
//				"The prolog help will be opened in the background.");
	}

}

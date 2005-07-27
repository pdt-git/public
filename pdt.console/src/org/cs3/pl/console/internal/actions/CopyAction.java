package org.cs3.pl.console.internal.actions;

import org.cs3.pl.console.SelectionContextAction;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.custom.StyledText;

public class CopyAction extends Action implements SelectionContextAction {

	private StyledText output;


	public boolean validate() {
		return output.getSelectionText().length() != 0;
	}

	public void init(StyledText output) {
		this.output = output;
	}

	public void run() {
		output.copy();
	}

}

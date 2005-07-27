package org.cs3.pdt.console.internal.actions;

import org.cs3.pl.console.SelectionContextAction;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.custom.StyledText;

public class SelectAllAction extends Action implements SelectionContextAction {

	private StyledText output;


	public boolean validate() {
		return output.getText().length() != 0;
	}

	public void init(StyledText output) {
		this.output = output;
	}

	public void run() {
		output.selectAll();
	}

}

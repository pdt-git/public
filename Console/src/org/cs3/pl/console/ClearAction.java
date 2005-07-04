package org.cs3.pl.console;

import org.eclipse.jface.action.Action;
import org.eclipse.swt.custom.StyledText;

public class ClearAction extends Action implements SelectionContextAction {

	private StyledText output;


	public boolean validate() {
		return output.getText().length() != 0;
	}

	public void init(StyledText output) {
		this.output = output;
	}

	public void run() {
		output.setText("");
	}

}

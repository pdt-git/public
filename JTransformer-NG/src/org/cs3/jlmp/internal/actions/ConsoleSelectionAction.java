package org.cs3.jlmp.internal.actions;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.SelectionContextAction;
import org.eclipse.jface.action.Action;
import org.eclipse.swt.custom.StyledText;

abstract class ConsoleSelectionAction extends Action implements SelectionContextAction {

	private String line;
	private String selection;
	public void init(StyledText output) {
		selection = output.getSelectionText();
		int lineAtOffset = output.getLineAtOffset(output.getCaretOffset());
		int lineOffset = output.getOffsetAtLine(lineAtOffset);
		int nextLineOffset;
		if(output.getLineCount() > lineAtOffset+1) 
		   nextLineOffset = output.getOffsetAtLine(lineAtOffset+1);
		else
			nextLineOffset =output.getText().length(); 
		if(nextLineOffset > lineOffset)
			line = output.getTextRange(lineOffset, nextLineOffset-lineOffset);
		else
			line = "";
	}

		public boolean validate() {
			try {
				getPefId();
				return true;
			} catch (NumberFormatException nfe) {
				// no source location found or not an ast id 
				Debug.report(nfe);
				return false;
			}
		}

		protected int getPefId() {
			String idString = removeTrailingComma(selection);
			return Integer.parseInt(idString);
		}

		protected String removeTrailingComma(String string) {
			return string.replaceAll(",", "").replaceAll(" ", "");
		}

		public String getLine() {
			return line;
		}

		public String getSelection() {
			return selection;
		}



}

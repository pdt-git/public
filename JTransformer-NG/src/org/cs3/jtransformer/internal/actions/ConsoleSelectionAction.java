package org.cs3.jtransformer.internal.actions;

import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;

abstract class ConsoleSelectionAction implements IViewActionDelegate {

	private String line="";
	private String selection="";
	private PrologConsole console;
		protected int getPefId() {
			String idString = removeTrailingComma(getSelection());
			return Integer.parseInt(idString);
		}

		protected String removeTrailingComma(String string) {
			return string.replaceAll(",", "").replaceAll(" ", "");
		}

		

		

		private void setSelection(String selection) {
			this.selection = selection;
		}

		protected String getSelection() {
			return selection;
		}

		public void init(IViewPart view) {
			this.console=(PrologConsole) view;
			
		}

		

		public void selectionChanged(IAction action, ISelection selection) {
			if(selection instanceof ITextSelection){
				ITextSelection s = (ITextSelection) selection;
				setSelection(s.getText());
			}
			
		}

		

		protected String getLine() {
			int lineAtOffset = console.getLineAtOffset(console.getCaretOffset());
			int lineOffset = console.getOffsetAtLine(lineAtOffset);
			int nextLineOffset;
			if(console.getLineCount() > lineAtOffset+1) 
			   nextLineOffset = console.getOffsetAtLine(lineAtOffset+1);
			else
				nextLineOffset =console.getText().length(); 
			if(nextLineOffset > lineOffset)
				return console.getTextRange(lineOffset, nextLineOffset-lineOffset);
			else
				return "";
		}



}

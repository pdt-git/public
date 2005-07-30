package org.cs3.pdt.internal.actions;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.PDTUtils;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public class ConsoleErrorMessageGotoLineAction extends Action {

	private StyledText text;
	private int targetLine = -1;
	private String filename;


	public boolean validate() {
		try {
			int lineAtOffset = text.getLineAtOffset(text.getCaretOffset());
			int lineOffset = text.getOffsetAtLine(lineAtOffset);
			int nextLineOffset;
			if(text.getLineCount() > lineAtOffset+1) 
			   nextLineOffset = text.getOffsetAtLine(lineAtOffset+1);
			else
				nextLineOffset =text.getText().length();
			String line;
			if(nextLineOffset > lineOffset)
				line = text.getTextRange(lineOffset, nextLineOffset-lineOffset);
			else
				line = "";
		
			int start = line.indexOf(".pl:") + 4;
			int end = start;
			while(line.charAt(end) >= '0' && line.charAt(end) <= '9')
				end ++;

			//				internal line numbers start with 0
			targetLine =  Integer.parseInt(line.substring(start,end))-1;  
			filename = getReferencedFilename(line);
		} catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}

	public void run(){
		try {
			gotoLocation(filename,targetLine);
		} catch (PartInitException e) {
			e.printStackTrace();
			Debug.report(e);
		} catch (IOException e) {
			e.printStackTrace();
			Debug.report(e);
		} catch (BadLocationException e) {
			e.printStackTrace();
			Debug.report(e);
		}
	}
	
	public String getReferencedFilename(String line) {
		int start = 0;
		while(notPartOfFileName(line.charAt(start)))
			start++;
		while(notStartOfFileName(line,start))
			start++;
		int end = line.indexOf(".pl:") + 3;
		return line.substring(start, end);
		
	}

	private boolean notStartOfFileName(String line, int offset) {
		if(line.charAt(offset) == '/')
			return false;
		if(line.length() > offset+2 && 
				line.charAt(offset + 1) == ':' &&
				line.charAt(offset + 2) == '/')
			return false;
		return true;
	}


	private boolean notPartOfFileName(char c) {
		switch(c) {
			case ' ':
			case '(':
			case '\t':
			case '\n':
			case '\r':
				return true;
			default:
				return false;
		}
	}

	public void init(StyledText styledText) {
		this.text = styledText;
		
	}

	public void gotoLocation(String filename, int line) throws IOException, PartInitException, BadLocationException {

		Debug.debug("gotoLocation got this filename: "+filename);
		try {
			filename = (new File(filename).getCanonicalPath());
		} catch (IOException e3) {
			Debug.report(e3);
		}
		Debug.debug("and now, gotoLocation got this filename: "+filename);		
//		IPath path = new Path(filename);
//		FileLocationRetriever retriever  = new FileLocationRetriever(PDTPlugin.getDefault());
		try {
			IFile file = PDTUtils.findFileForLocation(filename);
	//		file = retriever.fileForLocation(path);
			
			IWorkbenchPage page= PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			IDE.openEditor(page, file, true);
			
			IDocument document = ((AbstractTextEditor)page.getActiveEditor()).
					getDocumentProvider().getDocument(page.getActiveEditor().getEditorInput());
			IRegion region = document.getLineInformation(line);
			ISelection selection = new TextSelection(document,region.getOffset(),region.getLength());
			page.getActiveEditor().getEditorSite().getSelectionProvider().setSelection(selection);
		} catch(final Exception ex){
			ex.printStackTrace();
			final Display display = PlatformUI.getWorkbench().getDisplay();
			display.syncExec(new Runnable() {

				public void run() {
					MessageDialog.openError(display.getActiveShell(),
							"could not open file",
							ex.getLocalizedMessage()
							);
				}
				
			});
		}
		
	}
}

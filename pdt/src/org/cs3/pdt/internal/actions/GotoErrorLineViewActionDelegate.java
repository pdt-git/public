/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.internal.actions;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.AbstractTextEditor;

public class GotoErrorLineViewActionDelegate implements IViewActionDelegate{

	

	private PrologConsole console;
	private int targetLine;
	private String filename;
	@Override
	public void init(IViewPart view) {
		this.console= (PrologConsole) view;
	}

	@Override
	public void run(IAction action) {
		validate();
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

	@Override
	public void selectionChanged(IAction action, ISelection selection) {		
	}
	
	public boolean validate() {
		try {
			
			int lineAtOffset = console.getLineAtOffset(console.getCaretOffset());
			int lineOffset = console.getOffsetAtLine(lineAtOffset);
			int nextLineOffset;
			if(console.getLineCount() > lineAtOffset+1) 
			   nextLineOffset = console.getOffsetAtLine(lineAtOffset+1);
			else
				nextLineOffset =console.getText().length();
			String line;
			if(nextLineOffset > lineOffset)
				line = console.getTextRange(lineOffset, nextLineOffset-lineOffset);
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
			IFile file = PDTCoreUtils.findFileForLocation(filename);
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

				@Override
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

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

package org.cs3.pdt;

import java.io.IOException;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.FindReplaceDocumentAdapter;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;

public final class PDTUtils {

	public static PrologInterface getActiveConsolePif() {
		return PrologConsolePlugin.getDefault().getPrologConsoleService()
				.getActivePrologConsole().getPrologInterface();
	}

	public static void showSourceLocation(final SourceLocation loc) {
		if (Display.getCurrent() != UIUtils.getDisplay()) {

			UIUtils.getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					showSourceLocation(loc);
				}
			});
			return;
		}

		
		// For predicates implemented by external language code the Prolog side returns
		// an error message instead of a file name. Intercept and display it:
		if (loc.file
				.equals("No Prolog source code (only compiled external language code)")) {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			UIUtils.displayMessageDialog(
					shell,
					"External language predicate",
					"There is no Prolog source code for this predicate (only compiled external language code).");
			return;
		}
		
		IFile file = null;
		IPath fpath = new Path(loc.file);
		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		
		// see if it is a workspace path:
		file = wsRoot.getFile(fpath);

		
		if (!loc.isWorkspacePath) {
			try {
				file = PDTCoreUtils.findFileForLocation(loc.file);
			} catch(IllegalArgumentException iae){
				if(iae.getLocalizedMessage().startsWith("Not in Workspace")){
					openExternalFileInEditor(loc);
					return;
				} else {
					Debug.report(iae);
					throw new RuntimeException(iae);
				}
			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}

		try {
			IWorkbenchPage page = UIUtils.getActivePage();
			IEditorPart part= IDE.openEditor(page, file);
			openInEditor(loc, part);
		} catch (PartInitException e) {
			Debug.report(e);
			return;
		}


	}

	public static void openExternalFileInEditor(final SourceLocation loc) {
		IFileStore fileStore = EFS.getLocalFileSystem().getStore(new Path(loc.file));
		if (!fileStore.fetchInfo().isDirectory() && fileStore.fetchInfo().exists()) {
			try {
				IWorkbenchPage page = UIUtils.getActivePage();
		        IEditorPart part = IDE.openEditorOnFileStore(page, fileStore);
				openInEditor(loc, part);
			} catch (PartInitException e) {
				Debug.report(e);
			}
			return;
		}
	}

	private static void openInEditor(final SourceLocation loc,IEditorPart part) {
		if (part instanceof PLEditor) {
			PLEditor editor = (PLEditor) part;

			IDocument doc = editor.getDocumentProvider().getDocument(
					editor.getEditorInput());
			int offset;
			if( loc.isLineLocation()){
				Document document = (Document) editor.getDocumentProvider().getDocument(editor.getEditorInput());
				try {
					offset = document.getLineOffset(loc.getLine()-1);
					if(loc.getPredicateName()!=null){
						FindReplaceDocumentAdapter finder = new FindReplaceDocumentAdapter(document);
						
						IRegion region= finder.find(offset, loc.getPredicateName(), true, true, true,false);
						if(region!=null){
							editor.gotoOffset(region.getOffset(),region.getLength());
							return;
						} 

					}
					editor.gotoOffset(offset, 0);
					return;
				} catch (BadLocationException e) {
					e.printStackTrace();
					return;
				}
			} else {
				offset=PDTCoreUtils.convertLogicalToPhysicalOffset(
						doc.get(), loc.getOffset());
				editor.gotoOffset(offset,loc.getEndOffset() - loc.getOffset());
				
			}
//			editor.gotoOffset(PDTCoreUtils.convertLogicalToPhysicalOffset(
//					doc.get(), loc.offset));

		}
	}

	public static void openEditorOnExternalFile(int currentOffset, int currentLength,
			boolean activate, IFile file) {
		PLEditor editor= null;
		try {
		    editor = (PLEditor) IDE.openEditor(UIUtils.getActivePage(),file);
		} catch (PartInitException e1) {
			return;
		}
		IDocument doc = editor.getDocumentProvider().getDocument(editor.getEditorInput());
		int endOffset=currentOffset+currentLength;
		currentOffset = PDTCoreUtils.convertLogicalToPhysicalOffset(doc.get(),
				currentOffset);
		endOffset = PDTCoreUtils.convertLogicalToPhysicalOffset(doc.get(),
				endOffset);
		currentLength=endOffset-currentOffset;
		if (editor != null && activate) {
			editor.getEditorSite().getPage().activate(editor);
			if (editor instanceof ITextEditor) {
				ITextEditor textEditor= editor;
				textEditor.selectAndReveal(currentOffset, currentLength);
			}
		}
	}

}
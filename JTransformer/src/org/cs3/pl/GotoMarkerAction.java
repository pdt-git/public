/*
 * Created on 01.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl;
/*******************************************************************************
 * Copyright (c) 2000, 2003 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials 
 * are made available under the terms of the Common Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/cpl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;

import org.eclipse.ui.IEditorDescriptor;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IEditorRegistry;
import org.eclipse.ui.IReusableEditor;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.AbstractTextEditor;

import org.eclipse.search.ui.ISearchResultView;
import org.eclipse.search.ui.ISearchResultViewEntry;
import org.eclipse.search.ui.SearchUI;

import org.eclipse.search.internal.ui.SearchMessages;
import org.eclipse.search.internal.ui.SearchPlugin;
import org.eclipse.search.internal.ui.util.ExceptionHandler;

public class GotoMarkerAction extends Action {

	private IEditorPart fEditor;

	public void run() {
		ISearchResultView view= SearchUI.getSearchResultView();		
		ISelection selection= view.getSelection();
		Object element= null;
		if (selection instanceof IStructuredSelection)
			element= ((IStructuredSelection)selection).getFirstElement();
		if (element instanceof ISearchResultViewEntry) {
			ISearchResultViewEntry entry= (ISearchResultViewEntry)element;
			show(entry.getSelectedMarker());
		}
	}

	private void show(IMarker marker) {
		if (SearchUI.reuseEditor())
			showWithReuse(marker);
		else
			showWithoutReuse(marker);
	}

	private void showWithReuse(IMarker marker) {
		IWorkbenchPage page= SearchPlugin.getActivePage();
		IResource resource= marker.getResource();
		if (page == null || !(resource instanceof IFile))
			return;
		
		IEditorInput input= new FileEditorInput((IFile)resource);
		String editorId= null;
		IEditorDescriptor desc= IDE.getDefaultEditor((IFile)resource);
		if (desc == null)
			editorId= SearchPlugin.getDefault().getWorkbench().getEditorRegistry().findEditor(IEditorRegistry.SYSTEM_EXTERNAL_EDITOR_ID).getId();
		else
			editorId= desc.getId();

		IEditorPart editor= page.findEditor(input);
		if (editor != null)
			page.bringToTop(editor);
		else {
			boolean isOpen= false;
			if (fEditor != null) {
				IEditorReference[] parts= page.getEditorReferences();
				int i= 0;
				while (!isOpen && i < parts.length)
					isOpen= fEditor == parts[i++].getEditor(false);
			}

			boolean canBeReused= isOpen && !fEditor.isDirty() && !isPinned(fEditor);
			boolean showsSameInputType= fEditor != null && fEditor.getSite().getId().equals(editorId);

			if (canBeReused && !showsSameInputType) {
				page.closeEditor(fEditor, false);
				fEditor= null;
			}
			
			if (canBeReused && showsSameInputType) {
				((IReusableEditor)fEditor).setInput(input);
				page.bringToTop(fEditor);
				editor= fEditor;
			} else
				try {
					editor= page.openEditor(input, editorId, false);
					if (editor instanceof IReusableEditor)
						fEditor= editor;
					else
						fEditor= null;
				} catch (PartInitException ex) {
					ExceptionHandler.handle(ex, SearchMessages.getString("Search.Error.openEditor.title"), SearchMessages.getString("Search.Error.openEditor.message")); //$NON-NLS-2$ //$NON-NLS-1$
					return;
				}
		}
		
		if (editor != null) {
			IFile iFile;
			try {
				if (marker.getAttribute(IMarker.CHAR_END) == null) {
					iFile = (IFile)marker.getAttribute("Resource");
					String line = (String)marker.getAttribute(IMarker.LINE_NUMBER);
					Integer pos = getLineNumber(editor,line, iFile);
					marker.getAttributes().put(IMarker.CHAR_START, pos);
					marker.getAttributes().put(IMarker.CHAR_END, pos);
				}
				IDE.gotoMarker(editor, marker);
			} catch (CoreException e) {
				Debug.report(e);
			}
		}

	}

	private boolean isPinned(IEditorPart editor) {
		if (editor == null)
			return false;
		
		IEditorReference[] editorRefs= editor.getEditorSite().getPage().getEditorReferences();
		int i= 0;
		while (i < editorRefs.length) {
			if (editor.equals(editorRefs[i].getEditor(false)))
				return editorRefs[i].isPinned();
			i++;
		}
		return false;
	}
	
	private void showWithoutReuse(IMarker marker) {
		IWorkbenchPage page= SearchPlugin.getActivePage();
		if (page == null)
			return;

		try {
			IDE.openEditor(page, marker, false);
		} catch (PartInitException ex) {
			ExceptionHandler.handle(ex, SearchMessages.getString("Search.Error.openEditor.title"), SearchMessages.getString("Search.Error.openEditor.message")); //$NON-NLS-2$ //$NON-NLS-1$
			return;
		}
	}

	private Integer getLineNumber(IEditorPart editor, String line, IFile iFile) {
		FileEditorInput fei = new FileEditorInput(iFile);
		IDocument document;
		document =((AbstractTextEditor)editor).getDocumentProvider().getDocument(fei);
//		System.out.println("namen: "+getEditorInput().getName());
		int offset;
			try {
				offset = document.getLineInformation(Integer.parseInt(line)-1).getOffset();
				return new Integer(offset);
			} catch (NumberFormatException e) {
				Debug.report(e);
			} catch (BadLocationException e) {
				Debug.report(e);
			}
		return new Integer(0);
	}

}

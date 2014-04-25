/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.internal.editors;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;

import org.cs3.pdt.PDTPredicates;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.lifecycle.PrologEventDispatcher;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceEvent;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.PrologInterfaceListener;
import org.cs3.prolog.pif.service.ActivePrologInterfaceListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;

public class CurrentPifListener implements PrologInterfaceListener, ActivePrologInterfaceListener {

	private static final String FILE_LOADED = "file_loaded";

	@Override
	public void update(PrologInterfaceEvent e) {
		if (e.getSubject().equals(FILE_LOADED)) {
			fileLoaded(e.getEvent());
		} else if (e.getSubject().equals(PDTPredicates.PDT_EDIT_HOOK)) {
			openFileInEditor(e.getEvent());
		}
	}

	private void fileLoaded(String file) {
		file = Util.unquoteStringOrAtom(file);
		String[] parts = file.split("<>");
		for (String s : parts) {
			currentPif.addConsultedFile(s);
		}
	}

	public void openFileInEditor(String event) {
		if (event.startsWith("'")) {
			event = event.substring(1);
		}
		if (event.endsWith("'")) {
			event = event.substring(0, event.length() - 1);
		}
		int index = event.lastIndexOf(" ");
		final String fileName = event.substring(0, index);
		final int lineNumber = Integer.parseInt(event.substring(index + 1));
		Debug.debug("Try to open PLEditor for " + fileName + " (line " + lineNumber + ")");
		Runnable r = new Runnable() {
			@Override
			public void run() {
				IEditorPart editorPart = PDTCommonUtil.openInEditor(fileName);
				if (editorPart != null && editorPart instanceof PLEditor){
					((PLEditor) editorPart).gotoLine(lineNumber);
				}
			}
		};
		Display.getDefault().syncExec(r);
	}

	PrologInterface currentPif;
	private PrologEventDispatcher currentDispatcher;
	
	private void addPifListener() {
		if (currentPif != null) {
			Debug.debug("add edit registry listener for pif " + currentPif.toString());
			currentDispatcher = new PrologEventDispatcher(currentPif,PrologRuntimeUIPlugin.getDefault().getLibraryManager());
			try {
				currentDispatcher.addPrologInterfaceListener(PDTPredicates.PDT_EDIT_HOOK, this);
				currentDispatcher.addPrologInterfaceListener(FILE_LOADED, this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
			
		}
	}
	private void removePifListener() {
		if (currentPif != null && currentDispatcher != null) {
			Debug.debug("remove edit registry listener for pif " + currentPif.toString());
			try {
				currentDispatcher.removePrologInterfaceListener(PDTPredicates.PDT_EDIT_HOOK, this);
				currentDispatcher.removePrologInterfaceListener(FILE_LOADED, this);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
		}
	}

	private void updateEntryPoints() {
		try {
			currentPif.queryOnce(bT(PDTCommonPredicates.REMOVE_ENTRY_POINTS, "_"));
			
			for (IFile file : PDTCommonPlugin.getDefault().getEntryPoints()) {
				try {
					String prologFileName = Util.prologFileName(file.getLocation().toFile().getCanonicalFile());
					currentPif.queryOnce(bT(PDTCommonPredicates.ADD_ENTRY_POINT, Util.quoteAtom(prologFileName)));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
		
	}

	@Override
	public void activePrologInterfaceChanged(PrologInterface pif) {
		if (currentPif == pif) {
			return;
		}
		
		removePifListener();
		
		currentPif = pif;
		
		addPifListener();
		
		updateEntryPoints();
	}

}



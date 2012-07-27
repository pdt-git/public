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

import java.io.File;

import org.cs3.prolog.common.Util;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.part.FileEditorInput;

public class PDTChangedFileInformation implements ISelection{
	private IEditorInput editorInput;
	
	public PDTChangedFileInformation(IEditorInput input) {
		editorInput = input;
	}
	
	public String getPrologFileName() {
		if( editorInput instanceof FileEditorInput) {
			FileEditorInput fileEditorInput = (FileEditorInput)editorInput;
			IPath path = fileEditorInput.getPath();
			File file = path.toFile();
			return Util.prologFileName(file);
		}
		if( editorInput instanceof FileStoreEditorInput) {
			FileStoreEditorInput e = (FileStoreEditorInput)editorInput;
			File file = new File(e.getURI());
			return Util.prologFileName(file);
		}
		return editorInput.getName();
	}

	@Override
	public boolean isEmpty() {
		return false;
	}
	
}



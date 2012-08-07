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

package org.cs3.pdt.actions;

import java.util.Map;

import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;

public class ApplyTransformationProcessorChangeCreator extends ChangeCreator{
	public ApplyTransformationProcessorChangeCreator(PrologRefactoringInfo info) {
		this.info = info;
	}

	@Override
	protected void getChangesFromSession(PrologSession session)
			throws PrologInterfaceException {
		modifiedFiles = session.queryAll("pdt_modified_file(File,Path)");
		textDeltas = session.queryAll("pdt_text_delta(File,Start,End,String)");
	}
	
	@Override
	protected void handleTextDeltas() throws CoreException{
		for (Map<String,Object> m : textDeltas) {
			Object key = m.get("File");
			int start = Integer.parseInt((String)m.get("Start"));
			int end = Integer.parseInt((String)m.get("End"));
			String string = (String) m.get("String");
			ReplaceEdit edit = new ReplaceEdit(start,end-start,string);
			fileChanges.get(key).addChild(edit);
		}
	}
	
	
	@Override
	protected void handleTextFileChanges() {
		for (Map<String,Object>fileEntry : modifiedFiles) {
			Object key = fileEntry.get("File");
			String path = Util.unquoteAtom( (String) fileEntry.get("Path"));
			IFile file = resolveFileIfPossible(path); 
			files.put(key, file);
			TextFileChange textFileChange = null;
			if (file != null) {
				textFileChange = new TextFileChange(file.getName(),file);
			}
			MultiTextEdit rootEdit = new MultiTextEdit();
			textFileChange.setEdit(rootEdit);
			fileChanges.put(key, rootEdit);
			change.add(textFileChange);
		}
	}

	@Override
	protected void handleResourceChanges() {
		;
	}

	
}



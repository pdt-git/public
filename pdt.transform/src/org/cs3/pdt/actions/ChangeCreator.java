package org.cs3.pdt.actions;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.cs3.pdt.transform.internal.RenameResourceChange;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;

public class ChangeCreator {

	protected PrologRefactoringInfo info;
	private RefactoringStatus status;
	protected List<Map<String,Object>> modifiedFiles;
	protected List<Map<String,Object>> textDeltas;
	protected List<Map<String,Object>> resourceDeltas;
	protected CompositeChange change;
	protected HashMap<Object, MultiTextEdit> fileChanges;
	protected HashMap<Object, IFile> files;

	public ChangeCreator() {
		super();
	}

	public Change createChange() throws CoreException,
			OperationCanceledException {
				resetFields();
				workWithSessionForChanges();
				handleResourceChanges();
				handleTextFileChanges();
				handleTextDeltas();
				return change;
			}

	protected void handleTextDeltas() throws CoreException {
		for (Map<String,Object> m : textDeltas) {
			Object key = m.get("File");
			int start = Integer.parseInt((String)m.get("Start"));
			int end = Integer.parseInt((String)m.get("End"));
			String string = (String) m.get("String");
			ReplaceEdit edit = new ReplaceEdit(start,end-start,string);
			fileChanges.get(key).addChild(edit);
		}
	}

	protected void resetFields() {
		status = new RefactoringStatus();
		modifiedFiles = null;
		textDeltas = null;
		resourceDeltas = null;
		change = new CompositeChange("apply transformations");
		fileChanges = new HashMap<Object, MultiTextEdit>();
		files = new HashMap<Object, IFile>();
	}

	protected void workWithSessionForChanges() {
		PrologSession session = null;
		try {
			try {
				session=info.getPrologInterace().getSession(PrologInterface.NONE);
				getChangesFromSession(session);
			} finally {
				if(session!=null){
				session.queryOnce("pdt_rollback_transformation('"
						+ info.getRefactoringId() + "')");
				}
			}
		} catch (PrologInterfaceException e) {
			status.addFatalError("Problems with Prolog Connection");
		} finally {
			if (session != null) {
				session.dispose();
			}
		}
	}

	protected void getChangesFromSession(PrologSession session)
	throws PrologInterfaceException {
		resourceDeltas = session.queryAll("pdt_resource_delta(Old,New,Type)");
		modifiedFiles = session.queryAll("pdt_modified_file(File,Path)");
		textDeltas = session.queryAll("pdt_text_delta(File,Start,End,String)");
	}

	
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

	protected void handleResourceChanges() {
		for (Map<String,Object> deltas : resourceDeltas) {
			String type = (String)deltas.get("Type");
			if ("rename".equals(type)) {
				String oldPath = Util.unquoteAtom((String) deltas.get("Old"));
				String newName = Util.unquoteAtom((String) deltas.get("New"));
				IFile file = resolveFileIfPossible(oldPath);
				RenameResourceChange newChange = new RenameResourceChange(null, file, newName,"no comment");
				change.add(newChange);
			} else {
				throw new RuntimeException("unsupported resource delta type: "
						+ type);
			}
	
		}
	}

	protected IFile resolveFileIfPossible(String path) {
		IFile file = null;
		try {
			file = PDTCoreUtils.findFileForLocation(path);
		} catch (IOException e) {
			Debug.report(e);
			status.addFatalError("Could not resolve file in workspace.");
		}
		return file;
	}

}
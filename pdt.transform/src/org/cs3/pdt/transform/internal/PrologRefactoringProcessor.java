package org.cs3.pdt.transform.internal;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.RefactoringParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.SharableParticipants;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.text.edits.ReplaceEdit;

public class PrologRefactoringProcessor extends RefactoringProcessor {
	
	private final PrologRefactoringInfo info;

	public PrologRefactoringProcessor(PrologRefactoringInfo info) {
		this.info = info;
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm,
			CheckConditionsContext context) throws CoreException,
			OperationCanceledException {
		
		
RefactoringStatus result = new RefactoringStatus();
		
		PrologSession s = null;
		Map m = null;
		List l = null;
		try{
				s=info.getPrologInterace().getSession();
				String query = "pdt_check_conditions("+info.getHead()+",Severity,Message)";
				l= s.queryAll(query);
								
		} catch (PrologInterfaceException e) {
			result.addFatalError("Problems with Prolog Connection");
		}finally{
			if(s!=null){
				s.dispose();
			}
		}
		for (Object object : l) {
			m = (Map) object;
			String severity = (String) m.get("Severity");
			String message = (String) m.get("Message");
			if("info".equals(severity)){
				result.addInfo(message);
			}
			else if("warning".equals(severity)){
				result.addWarning(message);
			}
			else if("error".equals(severity)){
				result.addError(message);
			}
			else if("fatal".equals(severity)){
				result.addFatalError(message);
			}
		}
		result.addInfo("Nota bene: Now it is final!");		
		return result;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		RefactoringStatus result = new RefactoringStatus();
		result.addInfo("Nota bene: I checked the initial conditions.");
		
		
		return result;
	}

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException,
			OperationCanceledException {
		RefactoringStatus status = new RefactoringStatus();
		PrologSession s = null;
		List l1 = null;
		List l2 = null;
		List l0=null;
		try{
				s=info.getPrologInterace().getSession();
				s.queryAll("pdt_perform_change("+info.getHead()+")");
				l0 = s.queryAll("pdt_resource_delta(Old,New,Type)");
				l1 =s.queryAll("pdt_modified_file(File,Path)");
				l2 = s.queryAll("pdt_text_delta(File,Start,End,String)");
				s.queryAll("pdt_cleanup_transformation("+info.getHead()+")");
				
		} catch (PrologInterfaceException e) {
			status.addFatalError("Problems with Prolog Connection");
		}finally{
			if(s!=null){
				s.dispose();
			}
		}				
		CompositeChange change = new CompositeChange("apply transformations");
		
		// resource changes first
		for (Object object : l0) {
			Map m = (Map) object;
			String type = (String)m.get("Type");
			if("rename".equals(type)){
				String oldPath = (String)m.get("Old");
				String newName = (String)m.get("New");
				IFile file=null;
				try {
					file = PDTCoreUtils.findFileForLocation(oldPath);
				} catch (IOException e) {
					Debug.report(e);
					status.addFatalError("Could not resolve file in workspace.");
				}
				change.add(new RenameResourceChange(null,file,newName, "no comment"));
			}
			else{
				throw new RuntimeException("unsupported resource delta type: "+type);
			}
			
			
		}
		// build map
		//   pef_file --> TextFileChange
		//HashMap<Object, IFile> files = new HashMap<Object, IFile>();
		HashMap<Object, MultiTextEdit> fileChanges = new HashMap<Object, MultiTextEdit>();
		HashMap<Object, IFile> files = new HashMap<Object, IFile>();
		
		for (Object object : l1) {
			Map m = (Map)object;
			Object key = m.get("File");
			String path = (String) m.get("Path");
			IFile file=null;
			try {
				file = PDTCoreUtils.findFileForLocation(path);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				Debug.report(e);
				continue;
			}
			TextFileChange textFileChange = new TextFileChange( file.getName(), file );
		    // a file change contains a tree of edits, first add the root of them
		    MultiTextEdit rootEdit = new MultiTextEdit();
		    textFileChange.setEdit(rootEdit);
			//files.put(key, file);
			fileChanges.put(key, rootEdit);
			files.put(key, file);
			change.add(textFileChange);
		}
		
		
		for (Object object : l2) {
			Map m = (Map)object;
			Object key = m.get("File");
			int start = Integer.parseInt((String)m.get("Start"));
			int end = Integer.parseInt((String)m.get("End"));
			IDocument doc = PDTCoreUtils.getDocument(files.get(key));
			start = PDTCoreUtils.convertCharacterOffset(doc, start);
			end = PDTCoreUtils.convertCharacterOffset(doc, end);
			String string = (String) m.get("String");
			ReplaceEdit edit = new ReplaceEdit(start,end-start,string);
			MultiTextEdit multiTextEdit = fileChanges.get(key);
			
			multiTextEdit.addChild(edit);
		}
		
		return change;
	}

	
	 
	
	@Override
	public Object[] getElements() {
		return new String[]{"Some Element"};
	}

	@Override
	public String getIdentifier() {

		return getClass().getName();
	}

	@Override
	public String getProcessorName() {

		return info.getName();
	}

	@Override
	public boolean isApplicable() throws CoreException {
		return true;
	}

	@Override
	public RefactoringParticipant[] loadParticipants(RefactoringStatus status,
			SharableParticipants sharedParticipants) throws CoreException {
		return new RefactoringParticipant[0];
	}

}

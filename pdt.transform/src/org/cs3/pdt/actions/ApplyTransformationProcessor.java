package org.cs3.pdt.actions;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
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

public class ApplyTransformationProcessor extends RefactoringProcessor {
	
	private final ApplyTransformationInfo info;

	public ApplyTransformationProcessor(ApplyTransformationInfo info) {
		this.info = info;
		// TODO Auto-generated constructor stub
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm,
			CheckConditionsContext context) throws CoreException,
			OperationCanceledException {
		RefactoringStatus result = new RefactoringStatus();
		
		result.addInfo("Nota bene: Now it is final!");
		
		return result;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
			throws CoreException, OperationCanceledException {
		RefactoringStatus result = new RefactoringStatus();
		if(info.pif==null){
			result.addFatalError("No Prolog process.");
		}
		PrologSession s = null;
		Map m = null;
		try{
				s=info.pif.getSession();	
				s.queryOnce("ensure_loaded(library('facade/pdt_facade'))");
				s.queryOnce("ensure_loaded(library('facade/pdt_delta'))");
				s.queryOnce("ensure_loaded(library('pef/pef_base'))");
				m =s.queryOnce("pef_count(pef_modified_file,C)");				
		} catch (PrologInterfaceException e) {
			result.addFatalError("Problems with Prolog Connection");
		}finally{
			if(s!=null){
				s.dispose();
			}
		}
		if(m==null){
			result.addFatalError("Query failed.");
			
		}
		else{
			int count = Integer.parseInt((String)m.get("C"));
			if(count>0){
				result.addInfo("Affected files: "+count);
			}
			else{
				result.addFatalError("No files were modified.");
			}
		}
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
		try{
				s=info.pif.getSession();				
				l1 =s.queryAll("pdt_modified_file(File,Path)");
				l2 = s.queryAll("pdt_text_delta(File,Start,End,String)");
				
		} catch (PrologInterfaceException e) {
			status.addFatalError("Problems with Prolog Connection");
		}finally{
			if(s!=null){
				s.dispose();
			}
		}				
		
		// build map
		//   pef_file --> TextFileChange
		//HashMap<Object, IFile> files = new HashMap<Object, IFile>();
		HashMap<Object, MultiTextEdit> fileChanges = new HashMap<Object, MultiTextEdit>();
		CompositeChange change = new CompositeChange("apply transformations");
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
			change.add(textFileChange);
		}
		
		
		for (Object object : l2) {
			Map m = (Map)object;
			Object key = m.get("File");
			int start = Integer.parseInt((String)m.get("Start"));
			int end = Integer.parseInt((String)m.get("End"));
			String string = (String) m.get("String");
			ReplaceEdit edit = new ReplaceEdit(start,end-start,string);
			fileChanges.get(key).addChild(edit);
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

		return "Apply Transformation";
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

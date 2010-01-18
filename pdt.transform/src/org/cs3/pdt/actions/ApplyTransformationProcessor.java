package org.cs3.pdt.actions;

import java.util.Map;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.participants.CheckConditionsContext;
import org.eclipse.ltk.core.refactoring.participants.RefactoringParticipant;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;
import org.eclipse.ltk.core.refactoring.participants.SharableParticipants;

public class ApplyTransformationProcessor extends RefactoringProcessor {
	
	final ApplyTransformationInfo info;
	private ChangeCreator changeCreator;

	public ApplyTransformationProcessor(ApplyTransformationInfo info) {
		this.info = info;
		this.changeCreator = new ApplyTransformationProcessorChangeCreator(info);
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
		if(info.getPrologInterface()==null){
			result.addFatalError("No Prolog process.");
		}
		PrologSession session = null;
		Map<String,Object> modifiedFiles = null;
		try{
				session=info.getPrologInterface().getSession(PrologInterface.NONE);	
				session.queryOnce("ensure_loaded(library('facade/pdt_facade'))");
				session.queryOnce("ensure_loaded(library('facade/pdt_delta'))");
				session.queryOnce("ensure_loaded(library('pef/pef_base'))");
				modifiedFiles =session.queryOnce("pef_count(pef_modified_file,C)");				
		} catch (PrologInterfaceException e) {
			result.addFatalError("Problems with Prolog Connection");
		}finally{
			if(session!=null){
				session.dispose();
			}
		}
		if(modifiedFiles==null){
			result.addFatalError("Query failed.");
			
		}
		else{
			int count = Integer.parseInt((String)modifiedFiles.get("C"));
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
				return changeCreator.createChange();
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

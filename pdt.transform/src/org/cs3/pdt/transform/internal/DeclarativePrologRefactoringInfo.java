package org.cs3.pdt.transform.internal;

import java.io.File;

import org.cs3.pdt.transform.PrologRefactoringDescriptor;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.FileSearchPathConfigurator;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPart;

public class DeclarativePrologRefactoringInfo extends PrologRefactoringInfo {
	private PrologRefactoringDescriptor descriptor;	
	private Option[] options;
	private String selectionTerm;
	private PrologInterface pif;

	public DeclarativePrologRefactoringInfo(PrologRefactoringDescriptor desc, ISelection selection,IWorkbenchPart activePart) throws CoreException {
		this.descriptor=desc;		
		this.options=descriptor.getParameters(selection, activePart);
		this.selectionTerm=descriptor.getSelectionTerm(selection, activePart);
		this.pif=descriptor.getPrologInterface(selection, activePart);
	}
	
	@Override
	public String getRefactoringId() {
		return descriptor.getId();
	}

	@Override
	public String getName() {
		return descriptor.getLabel();
	}

	@Override
	public PrologInterface getPrologInterace() {	
		return pif;
	}

	@Override
	public Option[] getOptions() {
		return options;
	}

	@Override
	public void configure(PrologLibraryManager libman, PrologSession s) throws PrologInterfaceException {		
		FileSearchPathConfigurator.configureFileSearchPath(libman, s, descriptor.getDependencies());
		File[] definitions = descriptor.getDefinitions();
		for (int i = 0; i < definitions.length; i++) {
			s.queryOnce("ensure_loaded('"+Util.prologFileName(definitions[i])+"')");	
		}
		
	}

	@Override
	public String getParameterTerm() {		
		return descriptor.getParametersTerm(parameters);
	}

	@Override
	public String getSelectionTerm() {
		return selectionTerm;
	}

	

}

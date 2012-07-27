package org.cs3.prolog.pif.service;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public interface ConsultListener {
	
	void beforeConsult(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException;
	void beforeConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException; 
	void afterConsult(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException;
	void afterConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException; 
	
}

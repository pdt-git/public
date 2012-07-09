package org.cs3.prolog.pif.service;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public interface PDTReloadExecutor {
	
	int getPriority();
	
	boolean executePDTReload(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException;
	boolean executePDTReload(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException; 
	
}

package org.cs3.pdt.connector.internal.service.ext;

import java.util.List;

import org.cs3.pdt.connector.service.IPrologProcessService;
import org.cs3.prolog.connector.process.PrologProcess;
import org.eclipse.core.resources.IFile;

public interface IPrologProcessServiceExtension extends IPrologProcessService {
	
	/**
	 * Consults a list of files into the given PrologProcess.
	 * 
	 * @param files
	 *            the list of files
	 * @param pif
	 *            the PrologProcess
	 */
	void consultFilesSilent(List<IFile> files, PrologProcess pif);

}

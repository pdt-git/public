package org.cs3.pdt.connector.internal.service.ext;

import java.util.List;

import org.cs3.pdt.connector.service.IPrologInterfaceService;
import org.cs3.prolog.connector.process.PrologInterface;
import org.eclipse.core.resources.IFile;

public interface IPrologInterfaceServiceExtension extends IPrologInterfaceService {
	
	/**
	 * Consults a list of files into the given PrologInterface.
	 * 
	 * @param files
	 *            the list of files
	 * @param pif
	 *            the PrologInterface
	 */
	void consultFilesSilent(List<IFile> files, PrologInterface pif);

}

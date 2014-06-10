package org.cs3.prolog.internal.pif.service.ext;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.service.IPrologInterfaceService;
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

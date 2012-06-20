package org.cs3.prolog.pif.service;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.eclipse.core.resources.IFile;

public interface IPrologInterfaceService {

	void registerPrologInterfaceProvider(PrologInterfaceProvider provider);

	void unRegisterPrologInterfaceProvider(PrologInterfaceProvider provider);

	PrologInterface getPrologInterface();

	void registerPDTReloadExecutor(PDTReloadExecutor executor);

	void unRegisterPDTReloadExecutor(PDTReloadExecutor executor);

	void registerConsultListener(ConsultListener listener);

	void unRegisterConsultListener(ConsultListener listener);

	void consultFile(IFile file);

	void consultFile(String file);

	void consultFiles(List<IFile> files);
	
}

/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.pif.service;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.eclipse.core.resources.IFile;

public interface IPrologInterfaceService {

	void registerPDTReloadExecutor(PDTReloadExecutor executor);

	void unRegisterPDTReloadExecutor(PDTReloadExecutor executor);

	void registerConsultListener(ConsultListener listener);

	void unRegisterConsultListener(ConsultListener listener);

	void consultFile(IFile file);

	void consultFile(String file);

	void consultFiles(List<IFile> files);

	void registerActivePrologInterfaceListener(ActivePrologInterfaceListener listener);

	void unRegisterActivePrologInterfaceListener(ActivePrologInterfaceListener listener);

	PrologInterface getActivePrologInterface();

	void setActivePrologInterface(PrologInterface pif);
	
}



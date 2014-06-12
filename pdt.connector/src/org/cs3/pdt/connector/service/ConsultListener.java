/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector.service;

import java.util.List;

import org.cs3.prolog.connector.process.PrologProcess;
import org.cs3.prolog.connector.process.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * This interface is used to listen to the consulting process of an
 * {@link IPrologInterfaceService}.
 */
public interface ConsultListener {

	/**
	 * A list of files which will be consulted into the given
	 * {@link PrologProcess}
	 * 
	 * @param pif
	 *            the given {@link PrologProcess}
	 * @param files
	 *            the list of files
	 * @param monitor
	 *            a progress monitor
	 * @throws PrologInterfaceException
	 */
	void beforeConsult(PrologProcess pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException;

	/**
	 * A list of toplevel files has been be consulted successfully into the given
	 * {@link PrologProcess}
	 * 
	 * @param pif
	 *            the given {@link PrologProcess}
	 * @param files
	 *            the list of toplevel files
	 * @param monitor
	 *            a progress monitor
	 * @param allConsultedFiles
	 *            the list of all consulted files
	 * @throws PrologInterfaceException
	 */
	void afterConsult(PrologProcess pif, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologInterfaceException;

}

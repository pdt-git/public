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

import org.cs3.prolog.common.Util;
import org.cs3.prolog.pif.PrologInterface;
import org.eclipse.core.resources.IFile;

/**
 * An IPrologInterfaceService manages an active {@link PrologInterface} and
 * consults files into this active PrologInterface.<br/>
 * The active PrologInterface can be accessed and set.
 * {@link ActivePrologInterfaceListener}s can be registered to listen to each
 * change of the active PrologInterface.<br/>
 * Consults can be triggered via the <code>consultFile(s)</code> methods.
 * The consult will be done in the active PrologInterface or in a given PrologInterface. The call of the
 * consult predicate <code>pdt_reload/1</code> is executed by the registered
 * {@link PDTReloadExecutor} with the highest priority. If an executor fails,
 * the next one will be tried out. Utility methods to retrieve file names
 * conforming to prolog syntax can be found in {@link Util}. Registered
 * {@link ConsultListener}s are notified before and after executing
 * <code>pdt_reload/1</code>.<br/>
 * This service can be acquired via
 * 
 * <pre>
 * PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
 * </pre>
 * 
 */
public interface IPrologInterfaceService {

	/**
	 * Registers an {@link PDTReloadExecutor}.
	 * 
	 * @param executor
	 *            the executor
	 */
	void registerPDTReloadExecutor(PDTReloadExecutor executor);

	/**
	 * Unregisters an {@link PDTReloadExecutor}.
	 * 
	 * @param executor
	 *            the executor
	 */
	void unRegisterPDTReloadExecutor(PDTReloadExecutor executor);

	/**
	 * Unregisters an {@link ConsultListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void registerConsultListener(ConsultListener listener);

	/**
	 * Unregisters an {@link ConsultListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void unRegisterConsultListener(ConsultListener listener);

	/**
	 * Consults a file into the active PrologInterface.
	 * 
	 * @param file
	 *            the file
	 */
	void consultFile(IFile file);

	/**
	 * Consults a file into the given PrologInterface.
	 * 
	 * @param file
	 *            the file
	 * @param pif
	 *            the PrologInterface
	 */
	void consultFile(IFile file, PrologInterface pif);

	/**
	 * Consults a file into the active PrologInterface.
	 * 
	 * @param file
	 *            the file
	 */
	void consultFile(String file);

	/**
	 * Consults a file into the given PrologInterface.
	 * 
	 * @param file
	 *            the file
	 * @param pif
	 *            the PrologInterface
	 */
	void consultFile(String file, PrologInterface pif);
	
	/**
	 * Consults a list of files into the active PrologInterface.
	 * 
	 * @param files
	 *            the list of files
	 */
	void consultFiles(List<IFile> files);

	/**
	 * Consults a list of files into the given PrologInterface.
	 * 
	 * @param files
	 *            the list of files
	 * @param pif
	 *            the PrologInterface
	 */
	void consultFiles(List<IFile> files, PrologInterface pif);
	
	/**
	 * Registers an {@link ActivePrologInterfaceListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void registerActivePrologInterfaceListener(ActivePrologInterfaceListener listener);

	/**
	 * Unregisters an {@link ActivePrologInterfaceListener}.
	 * 
	 * @param listener
	 *            the listener
	 */
	void unRegisterActivePrologInterfaceListener(ActivePrologInterfaceListener listener);

	/**
	 * Accesses the active PrologInterface.
	 * 
	 * @return the active PrologInterface
	 */
	PrologInterface getActivePrologInterface();

	/**
	 * Sets the active PrologInterface.
	 * 
	 * @param pif
	 *            the PrologInterface
	 */
	void setActivePrologInterface(PrologInterface pif);

}

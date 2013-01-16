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

package org.cs3.prolog.internal.pif.service;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.DefaultSubscription;
import org.cs3.prolog.connector.PrologConnectorPredicates;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.Subscription;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ActivePrologInterfaceListener;
import org.cs3.prolog.pif.service.ConsultListener;
import org.cs3.prolog.pif.service.IPrologInterfaceService;
import org.cs3.prolog.pif.service.PDTReloadExecutor;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;

public class PrologInterfaceService implements IPrologInterfaceService{
	
	public PrologInterfaceService() {
		registerPDTReloadExecutor(new DefaultReloadExecutor());
	}
	
	private PrologInterface activePrologInterface;
	
	private static final ISchedulingRule activePifChangedRule = new ISchedulingRule() {
		@Override
		public boolean isConflicting(ISchedulingRule rule) {
			return this == rule;
		}
		
		@Override
		public boolean contains(ISchedulingRule rule) {
			return this == rule;
		}
	};
	
	@Override
	public PrologInterface getActivePrologInterface() {
		if (activePrologInterface == null) {
			setActivePrologInterface(null);
		}
		return activePrologInterface;
	}
	
	@Override
	public synchronized void setActivePrologInterface(PrologInterface pif) {
		if (pif == null) {
			activePrologInterface = getDefaultPrologInterface();
		} else {
			if (activePrologInterface == pif) {
				return;
			} else {
				activePrologInterface = pif;
			}
		}
		fireActivePrologInterfaceChanged(activePrologInterface);
	}
	
	@SuppressWarnings("unchecked")
	private synchronized void fireActivePrologInterfaceChanged(final PrologInterface pif) {
		Job job = new Job("Active PrologInterface changed: notify listeners") {
			
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				ArrayList<ActivePrologInterfaceListener> listenersClone;
				synchronized (activePrologInterfaceListeners) {
					listenersClone = (ArrayList<ActivePrologInterfaceListener>) activePrologInterfaceListeners.clone();
				}
				
				monitor.beginTask("Active PrologInterface changed: notify listeners", listenersClone.size());
				
				for (ActivePrologInterfaceListener listener : listenersClone) {
					listener.activePrologInterfaceChanged(pif);
					monitor.worked(1);
				}
				monitor.done();
				return Status.OK_STATUS;
			}
		};
		job.setRule(activePifChangedRule);
		job.schedule();
	}
	
	private ArrayList<ActivePrologInterfaceListener> activePrologInterfaceListeners = new ArrayList<ActivePrologInterfaceListener>();
	
	@Override
	public void registerActivePrologInterfaceListener(ActivePrologInterfaceListener listener) {
		synchronized (activePrologInterfaceListeners) {
			activePrologInterfaceListeners.add(listener);
		}
	}
	
	@Override
	public void unRegisterActivePrologInterfaceListener(ActivePrologInterfaceListener listener) {
		synchronized (activePrologInterfaceListeners) {
			activePrologInterfaceListeners.remove(listener);
		}
	}
	
	private static final String DEFAULT_PROCESS = "Default Process";
	
	private PrologInterface getDefaultPrologInterface() {
		PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		Subscription subscription = registry.getSubscription(DEFAULT_PROCESS);
		if (subscription == null) {
			subscription = new DefaultSubscription(DEFAULT_PROCESS + "_indepent", DEFAULT_PROCESS, "Independent prolog process", "Prolog");
			registry.addSubscription(subscription);
		}
		PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(subscription);
		return pif;
	}
	
	private TreeSet<PDTReloadExecutor> pdtReloadExecutors = new TreeSet<PDTReloadExecutor>(new Comparator<PDTReloadExecutor>() {
		@Override
		public int compare(PDTReloadExecutor o1, PDTReloadExecutor o2) {
			return o2.getPriority() - o1.getPriority();
		}
	});
	
	@Override
	public void registerPDTReloadExecutor(PDTReloadExecutor executor) {
		synchronized (pdtReloadExecutors) {
			pdtReloadExecutors.add(executor);
		}
	}
	
	@Override
	public void unRegisterPDTReloadExecutor(PDTReloadExecutor executor) {
		synchronized (pdtReloadExecutors) {
			pdtReloadExecutors.remove(executor);
		}
	}
	
	private HashSet<ConsultListener> consultListeners = new HashSet<ConsultListener>();
	
	@Override
	public void registerConsultListener(ConsultListener listener) {
		synchronized (consultListeners) {
			consultListeners.add(listener);
		}
	}
	
	@Override
	public void unRegisterConsultListener(ConsultListener listener) {
		synchronized (consultListeners) {
			consultListeners.remove(listener);
		}
	}
	
	@Override
	public void consultFile(String file) {
		consultFile(file, getActivePrologInterface());
	}

	@Override
	public void consultFile(String file, PrologInterface pif) {
		try {
			consultFile(FileUtils.findFileForLocation(file), pif);
		} catch (IOException e) {
			Debug.report(e);
			return;
		}
	}

	@Override
	public void consultFile(final IFile file) {
		consultFile(file, getActivePrologInterface());
	}
	
	@Override
	public void consultFile(IFile file, PrologInterface pif) {
		ArrayList<IFile> fileList = new ArrayList<IFile>();
		fileList.add(file);
		consultFiles(fileList, pif);
	}
	
	@Override
	public void consultFiles(List<IFile> files) {
		consultFiles(files, getActivePrologInterface());
	}
	
	@Override
	public void consultFiles(final List<IFile> files, final PrologInterface pif) {
		Job job = new Job("Consult " + files.size() + " file(s)") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					consultFilesImpl(files, pif, monitor);
				} catch (PrologInterfaceException e) {
					Debug.report(e);
					return Status.CANCEL_STATUS;
				} finally {
					monitor.done();
				}
				return Status.OK_STATUS;
			}
		};
		job.setRule(new MultiRule(files.toArray(new IFile[files.size()])));
		job.schedule();
	}
	
	@SuppressWarnings("unchecked")
	private void consultFilesImpl(List<IFile> files, PrologInterface pif, IProgressMonitor monitor) throws PrologInterfaceException {
		HashSet<ConsultListener> consultListenersClone;
		synchronized (consultListeners) {
			consultListenersClone = (HashSet<ConsultListener>) consultListeners.clone();
		}
		
		monitor.beginTask("Consult " +  files.size() + " file(s)", consultListenersClone.size() * 4);
		
		for (ConsultListener listener : consultListenersClone) {
			monitor.subTask("Notify Listener");
			listener.beforeConsult(pif, files, new SubProgressMonitor(monitor, 1));
		}
		
		monitor.subTask("Execute reload");
		boolean success = executeReload(pif, files, new SubProgressMonitor(monitor, consultListenersClone.size()));
		
		if (success) {
			monitor.subTask("Collect all consulted files");
			List<String> allConsultedFiles = collectConsultedFiles(pif, new SubProgressMonitor(monitor, consultListenersClone.size()));
			
			for (ConsultListener listener : consultListenersClone) {
				monitor.subTask("Notify Listener");
				listener.afterConsult(pif, files, allConsultedFiles, new SubProgressMonitor(monitor, 1));
			}
		}
		monitor.done();
	}
	
	private List<String> collectConsultedFiles(PrologInterface pif, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		
		List<String> result = new ArrayList<String>();
		
		List<Map<String, Object>> reloadedFiles = pif.queryAll(bT(PrologConnectorPredicates.RELOADED_FILE, "File"));
		for (Map<String, Object> reloadedFile : reloadedFiles) {
			result.add(reloadedFile.get("File").toString());
		}
		
		monitor.done();
		
		return result;
	}

	@SuppressWarnings("unchecked")
	private boolean executeReload(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		TreeSet<PDTReloadExecutor> executorsClone;
		synchronized (pdtReloadExecutors) {
			executorsClone = (TreeSet<PDTReloadExecutor>) pdtReloadExecutors.clone();
		}
		monitor.beginTask("Execute reload", executorsClone.size());
		for (PDTReloadExecutor executor : executorsClone) {
			monitor.subTask("Execute reload");
			boolean success = executor.executePDTReload(pif, files, new SubProgressMonitor(monitor, 1));
			if (success) {
				monitor.done();
				return true;
			}
		}
		monitor.done();
		return false;
	}
	
	
}



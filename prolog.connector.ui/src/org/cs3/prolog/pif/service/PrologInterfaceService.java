package org.cs3.prolog.pif.service;

import java.io.IOException;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.TreeSet;

import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.MultiRule;

public class PrologInterfaceService implements IPrologInterfaceService{
	
	public PrologInterfaceService() {
		registerPrologInterfaceProvider(new DefaultPrologInterfaceProvider());
		registerPDTReloadExecutor(new DefaultReloadExecutor());
	}
	
	private TreeSet<PrologInterfaceProvider> pifProviders = new TreeSet<PrologInterfaceProvider>(new Comparator<PrologInterfaceProvider>() {
		@Override
		public int compare(PrologInterfaceProvider p1, PrologInterfaceProvider p2) {
			return p2.getPriority() - p1.getPriority();
		}
	});
	
	@Override
	public void registerPrologInterfaceProvider(PrologInterfaceProvider provider) {
		synchronized (pifProviders) {
			pifProviders.add(provider);
		}
	}
	
	@Override
	public void unRegisterPrologInterfaceProvider(PrologInterfaceProvider provider) {
		synchronized (pifProviders) {
			pifProviders.remove(provider);
		}
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public PrologInterface getPrologInterface() {
		TreeSet<PrologInterfaceProvider> providersClone;
		synchronized (pifProviders) {
			providersClone = (TreeSet<PrologInterfaceProvider>) pifProviders.clone();
		}
		for (PrologInterfaceProvider provider : providersClone) {
			PrologInterface pif = provider.getPrologInterface();
			if (pif != null) {
				return pif;
			}
		}
		return null;
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
		try {
			consultFile(FileUtils.findFileForLocation(file));
		} catch (IOException e) {
			Debug.report(e);
			return;
		}
	}
	
	@Override
	public void consultFile(final IFile file) {
		Job job = new Job("Consult " + file.getFullPath().toString()) {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					consultFileImpl(file, monitor);
				} catch (PrologInterfaceException e) {
					Debug.report(e);
					return Status.CANCEL_STATUS;
				}
				return Status.OK_STATUS;
			}
		};
		job.setRule(file);
		job.schedule();
	}
	
	@SuppressWarnings("unchecked")
	private void consultFileImpl(IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		HashSet<ConsultListener> consultListenersClone;
		synchronized (consultListeners) {
			consultListenersClone = (HashSet<ConsultListener>) consultListeners.clone();
		}
		PrologInterface pif = getPrologInterface();
		if (pif == null) {
			return;
		}
		
		monitor.beginTask("Consult " +  file.getFullPath().toString(), consultListenersClone.size() * 3);

		for (ConsultListener listener : consultListenersClone) {
			monitor.subTask("Notify Listener");
			listener.beforeConsult(pif, file, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK));
		}
		boolean success = executeReload(pif, file, new SubProgressMonitor(monitor, consultListenersClone.size(), SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK));
		if (success) {
			for (ConsultListener listener : consultListenersClone) {
				monitor.subTask("Notify Listener");
				listener.afterConsult(pif, file, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK));
			}
		}
		monitor.done();
	}
	
	@SuppressWarnings("unchecked")
	private boolean executeReload(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		TreeSet<PDTReloadExecutor> executorsClone;
		synchronized (pdtReloadExecutors) {
			executorsClone = (TreeSet<PDTReloadExecutor>) pdtReloadExecutors.clone();
		}
		monitor.beginTask("Execute reload", executorsClone.size());
		for (PDTReloadExecutor executor : executorsClone) {
			monitor.subTask("Execute reload");
			boolean success = executor.executePDTReload(pif, file, new SubProgressMonitor(monitor, 1, SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK));
			if (success) {
				monitor.done();
				return true;
			}
		}
		monitor.done();
		return false;
	}
	
	@Override
	public void consultFiles(final List<IFile> files) {
		Job job = new Job("Consult " + files.size() + " file(s)") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				try {
					consultFilesImpl(files, monitor);
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
	private void consultFilesImpl(List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		HashSet<ConsultListener> consultListenersClone;
		synchronized (consultListeners) {
			consultListenersClone = (HashSet<ConsultListener>) consultListeners.clone();
		}
		PrologInterface pif = getPrologInterface();
		if (pif == null) {
			return;
		}
		
		monitor.beginTask("Consult " +  files.size() + " file(s)", consultListenersClone.size() * 3);
		
		for (ConsultListener listener : consultListenersClone) {
			monitor.subTask("Notify Listener");
			listener.beforeConsult(pif, files, new SubProgressMonitor(monitor, 1));
		}
		
		monitor.subTask("Execute reload");
		boolean success = executeReload(pif, files, new SubProgressMonitor(monitor, consultListenersClone.size()));
		
		if (success) {
			for (ConsultListener listener : consultListenersClone) {
				monitor.subTask("Notify Listener");
				listener.afterConsult(pif, files, new SubProgressMonitor(monitor, 1));
			}
		}
		monitor.done();
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

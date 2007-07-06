package org.cs3.pdt.core.internal.builder;

import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class UpdateMarkersJob extends Job implements PrologInterfaceListener{

	private final Set<IFile> files;
	private final IPrologProject plProject;
	private IProgressMonitor monitor;

	public UpdateMarkersJob(IPrologProject plProject,Set<IFile> files) {
		super("Updating Prolog Markers for project "+plProject.getProject().getName());
		this.plProject = plProject;
		this.files = files;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		this.monitor = monitor;
		try {
			plProject.getMetaDataEventDispatcher().addPrologInterfaceListener("builder(interprete(_))", this);
			AsyncPrologSession s = ((PrologInterface2) plProject.getMetadataPrologInterface()).getAsyncSession();
			s.addBatchListener(new DefaultAsyncPrologSessionListener());
			monitor.beginTask("updating", files.size());
			s.queryOnce("update_markers", "pdt_with_targets([problems],true)");
			s.join();			
			s.dispose();
			monitor.done();
			monitor=null;
			plProject.getMetaDataEventDispatcher().removePrologInterfaceListener("builder(interprete(_))", this);
		} catch (PrologInterfaceException e) {
			UIUtils.createErrorStatus(PDTCorePlugin.getDefault().getErrorMessageProvider(), e, PDTCore.ERR_PIF);
		}
		return new Status(IStatus.OK,PDTCore.PLUGIN_ID,"done");
	}

	public void update(PrologInterfaceEvent e) {
		if(monitor==null){
			return;
		}
		monitor.worked(1);
	}

}

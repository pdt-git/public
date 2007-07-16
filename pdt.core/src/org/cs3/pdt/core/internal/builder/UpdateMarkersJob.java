package org.cs3.pdt.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class UpdateMarkersJob extends Job implements PrologInterfaceListener {

	private final Set<IFile> files;
	private final IPrologProject plProject;
	private IProgressMonitor monitor;

	public UpdateMarkersJob(IPrologProject plProject, Set<IFile> files) {
		super("Updating Prolog Markers for project "
				+ plProject.getProject().getName());
		this.plProject = plProject;
		this.files = files;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		this.monitor = monitor;
		try {
			plProject.getMetaDataEventDispatcher().addPrologInterfaceListener(
					"builder(interprete(_))", this);
			AsyncPrologSession s = ((PrologInterface2) plProject
					.getMetadataPrologInterface()).getAsyncSession();
			s.addBatchListener(new DefaultAsyncPrologSessionListener());
			monitor.beginTask("updating", files.size());
			s.queryOnce("update_markers", "profile(pdt_with_targets([problems],true))");
			s.join();
			s.dispose();
			monitor.done();
			monitor = null;
			plProject.getMetaDataEventDispatcher()
					.removePrologInterfaceListener("builder(interprete(_))",
							this);
		} catch (PrologInterfaceException e) {
			UIUtils.createErrorStatus(PDTCorePlugin.getDefault()
					.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
		}
		return new Status(IStatus.OK, PDTCore.PLUGIN_ID, "done");
	}

	public void update(PrologInterfaceEvent e) {
		if (monitor == null) {
			return;
		}
		Map unifier = e.getUnifier();
		if (! e.getEvent().equals("done")) {
			return;
		}
		
		CCompound subject = (CCompound) PLUtil.createCTerm(e.getSubject());
		String plFile =  ((CCompound) subject.getArgument(0)).getArgument(0).getFunctorValue();

		try {
			IFile file = PDTCoreUtils.findFileForLocation(plFile);
			if (file != null && files != null && files.contains(file)) {
				monitor.worked(1);
			}
		} catch(IllegalArgumentException iae){
			//ignore files that are not in the workspace.
			//Debug.report(iae);
			;
		}
		  catch (IOException e1) {
			Debug.report(e1);
			// don't rethrow... this is only for progress reporting anyway.
		}

	}

}

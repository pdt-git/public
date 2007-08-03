package org.cs3.pdt.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.DefaultAsyncPrologSessionListener;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class UpdateMarkersJob extends Job implements PrologInterfaceListener {

	private final class _Listener extends DefaultAsyncPrologSessionListener {
		HashSet<Object> problemIds = new HashSet<Object>();

		@Override
		public void goalHasSolution(AsyncPrologSessionEvent e) {
			Map m = e.bindings;
			Object id = m.get("Id");
			String filename = (String) m.get("File");
			int start = Integer.parseInt(((String) m.get("Start")));
			int end = Integer.parseInt(((String) m.get("End")));
			String severity = (String) m.get("Severity");
			String message = (String) m.get("Msg");
			try {
				addMarker(filename, start, end, severity, message);
				if (markerMonitor == null) {
					markerMonitor = new SubProgressMonitor(monitor, 25,
							SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
					int work = Integer.parseInt((String) m.get("Count"));
					markerMonitor.beginTask("Creating Markers", work);
				}
				if (!problemIds.contains(id)) {
					problemIds.add(id);
					markerMonitor.worked(1);
				}
			} catch (CoreException e1) {
				UIUtils.logError(PDTCorePlugin.getDefault()
						.getErrorMessageProvider(), PDTCore.ERR_UNKNOWN,
						PDTCore.CX_UPDATE_MARKERS, e1);
			}
		}

		@Override
		public void goalSucceeded(AsyncPrologSessionEvent e) {
			if (markerMonitor != null) {
				markerMonitor.done();
			}
		}
		
		@Override
		public void goalFailed(AsyncPrologSessionEvent e) {
			UIUtils.logAndDisplayError(PDTCorePlugin.getDefault().getErrorMessageProvider(), UIUtils.getActiveShell(), PDTCore.ERR_QUERY_FAILED, PDTCore.CX_UPDATE_MARKERS, new RuntimeException("Query failed: "+e.query));
		}
		
		@Override
		public void goalRaisedException(AsyncPrologSessionEvent e) {
		
			UIUtils.logAndDisplayError(PDTCorePlugin.getDefault().getErrorMessageProvider(), UIUtils.getActiveShell(), PDTCore.ERR_QUERY_FAILED, PDTCore.CX_UPDATE_MARKERS, new RuntimeException("Goal raised exception: "+e.message+"\n query: "+e.query +"\n ticket: "+e.ticket));
		}
	}

	private final Set<IFile> files;
	private final IPrologProject plProject;
	private IProgressMonitor monitor;
	private IProgressMonitor buildMonitor;
	private IProgressMonitor markerMonitor;
	private Runnable finnish;

	public UpdateMarkersJob(IPrologProject plProject, Set<IFile> files,
			Runnable finnish) {
		super("Updating Prolog Markers for project "
				+ plProject.getProject().getName());
		this.plProject = plProject;
		this.files = files;
		this.finnish = finnish;
	}

	@Override
	protected IStatus run(IProgressMonitor monitor) {
		this.monitor = monitor;
		PrologSession session = null;

		try {
			plProject.getProject().deleteMarkers(PDTCore.PROBLEM, true,
					IResource.DEPTH_INFINITE);
			IPrologEventDispatcher dispatcher = PrologRuntimePlugin
					.getDefault().getPrologEventDispatcher(
							plProject.getMetadataPrologInterface());
			dispatcher.addPrologInterfaceListener("builder(interprete(_))",
					this);
			dispatcher.addPrologInterfaceListener("builder(problems)", this);
			PrologInterface2 pif = ((PrologInterface2) plProject
					.getMetadataPrologInterface());
			final AsyncPrologSession s = pif.getAsyncSession();
			s.addBatchListener(new _Listener());
			monitor.beginTask("updating", 100);
			buildMonitor = new SubProgressMonitor(monitor, 75,
					SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK);
			buildMonitor.beginTask("Searching for problems", files.size());
			s
					.queryAll(
							"update_markers",
							"pdt_with_targets([problems],(pdt_problem_count(Count),pdt_problem(Id,File,Start,End,Severity,Msg)))");
			while (!s.isIdle()) {
				if (UpdateMarkersJob.this.monitor.isCanceled()) {
					try {
						s.abort();
						UpdateMarkersJob.this.monitor.done();

					} catch (PrologInterfaceException e1) {
						Debug.rethrow(e1);
					}

				}
				try {
					Thread.sleep(1000);
				} catch (InterruptedException e1) {
					return UIUtils
							.createErrorStatus(PDTCorePlugin.getDefault()
									.getErrorMessageProvider(), e1,
									PDTCore.ERR_UNKNOWN);
				}
			}

			s.dispose();
			monitor.done();
			monitor = null;
			dispatcher.removePrologInterfaceListener("builder(interprete(_))",
					this);
		} catch (PrologInterfaceException e) {
			return UIUtils.createErrorStatus(PDTCorePlugin.getDefault()
					.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
		} catch (CoreException e) {
			return UIUtils.createErrorStatus(PDTCorePlugin.getDefault()
					.getErrorMessageProvider(), e, PDTCore.ERR_UNKNOWN);
		} finally {
			if (session != null) {
				session.dispose();
			}
			finnish.run();
		}
		return new Status(IStatus.OK, PDTCore.PLUGIN_ID, "done");
	}

	private void addMarker(String filename, int start, int end,
			String severity, String message) throws CoreException {
		IFile file = null;
		try {

			file = PDTCoreUtils.findFileForLocation(filename);

		} catch (IllegalArgumentException iae) {
			// ignore files that are not in the workspace.
			// Debug.report(iae);
			;
		} catch (IOException e) {
			Debug.rethrow(e);
		}
		if (file == null) {
			return;
		}
		IMarker marker = file.createMarker(PDTCore.PROBLEM);
		IDocument doc = PDTCoreUtils.getDocument(file);
		start = PDTCoreUtils.convertCharacterOffset(doc, start);
		end = Math
				.max(start + 1, PDTCoreUtils.convertCharacterOffset(doc, end));
		end = Math.min(doc.getLength(), end);

		MarkerUtilities.setCharStart(marker, start);
		MarkerUtilities.setCharEnd(marker, end);

		marker.setAttribute(IMarker.SEVERITY, mapSeverity(severity));
		marker.setAttribute(IMarker.MESSAGE, message);
	}

	private int mapSeverity(String severity) {
		if ("error".equals(severity)) {
			return IMarker.SEVERITY_ERROR;
		}
		if ("warning".equals(severity)) {
			return IMarker.SEVERITY_WARNING;
		}
		if ("info".equals(severity)) {
			return IMarker.SEVERITY_INFO;
		}

		throw new IllegalArgumentException("cannot map severity constant: "
				+ severity);
	}

	public void update(PrologInterfaceEvent e) {
		if (buildMonitor == null) {
			return;
		}
		Map unifier = e.getUnifier();
		if (!e.getEvent().equals("done")) {
			return;
		}

		if (e.getSubject().equals("builder(problems)")) {
			buildMonitor.done();
			return;
		}
		CCompound subject = (CCompound) PLUtil.createCTerm(e.getSubject());
		String plFile = ((CCompound) subject.getArgument(0)).getArgument(0)
				.getFunctorValue();

		try {
			IFile file = PDTCoreUtils.findFileForLocation(plFile);
			if (file != null && files != null && files.contains(file)) {
				buildMonitor.worked(1);
			}
		} catch (IllegalArgumentException iae) {
			// ignore files that are not in the workspace.
			// Debug.report(iae);
			;
		} catch (IOException e1) {
			Debug.report(e1);
			// don't rethrow... this is only for progress reporting anyway.
		}

	}

}

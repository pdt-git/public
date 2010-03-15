package org.cs3.pdt.core.internal.builder;

import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermUtil;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class UpdateMarkersJob extends Job implements PrologInterfaceListener {

	private IProgressMonitor monitor;
	private final IPrologProject plProject;

	private String query;
	private String tag;

	public UpdateMarkersJob(IPrologProject plProject, String tag) {
		super("Updating Prolog Markers for project "
				+ plProject.getProject().getName());
		this.plProject = plProject;
		this.tag = tag;

	}

	private static String getMarkerType(String tag) {
		return PDTCore.PROBLEM + "." + tag;
	}

	private void addMarker(String id, String filename, int start, int end,
			String severity, String message) throws CoreException {
		IFile file = null;
		try {

			file = PDTCoreUtils.findFileForLocation(filename);

		} catch (Throwable e) {
			Debug.report(e);
			Debug
					.warning("The following filename caused problems: "
							+ filename);
		}
		if (file == null) {
			return;
		}
		IMarker marker = file.createMarker(getMarkerType(tag));
		IDocument doc = PDTCoreUtils.getDocument(file);
		start = PDTCoreUtils.convertLogicalToPhysicalOffset(doc, start);
		end = Math.max(start + 1, PDTCoreUtils.convertLogicalToPhysicalOffset(
				doc, end));
		end = Math.min(doc.getLength(), end);

		MarkerUtilities.setCharStart(marker, start);
		MarkerUtilities.setCharEnd(marker, end);

		marker.setAttribute(IMarker.SEVERITY, mapSeverity(severity));
		marker.setAttribute(IMarker.MESSAGE, message);
		marker.setAttribute(PDTCore.PROBLEM_ID, id);
	}

	public void processSolution(Map<String,Object> solution) {

		String id = (String)solution.get("Id");
		String filename = Util.unquoteAtom((String) solution.get("File"));
		int start = 0;
		int end = 0;
		try {
			start = Integer.parseInt(((String) solution.get("Start")));
			end = Integer.parseInt(((String) solution.get("End")));
		} catch (NumberFormatException ee) {
			Debug.report(ee);
		}
		String severity = Util.unquoteAtom((String) solution.get("Severity"));
		String message = Util.unquoteAtom((String) solution.get("Msg"));
		try {
			addMarker(id, filename, start, end, severity, message);

		} catch (CoreException e1) {
		}
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

	
	protected IStatus run(IProgressMonitor monitor) {

		this.monitor = monitor;
		PrologSession s = null;
		try {

			IPrologEventDispatcher dispatcher = PrologRuntimeUIPlugin
					.getDefault().getPrologEventDispatcher(
							plProject.getMetadataPrologInterface());

			String subject = "progress(" + tag + ")";
			dispatcher.addPrologInterfaceListener(subject, this);
			PrologInterface pif = ((PrologInterface) plProject
					.getMetadataPrologInterface());
			s = pif.getSession(PrologInterface.NONE);

			query = "pdt_problem(Id,File," + tag + ",Start,End,Severity,Msg)";

			List<Map<String, Object>> solutions = s.queryAll(query);

			dispatcher.removePrologInterfaceListener(subject, this);
			plProject.getProject().deleteMarkers(getMarkerType(tag), true,
					IResource.DEPTH_INFINITE);
			for (Map<String, Object> nextSolution : solutions) {
				processSolution(nextSolution);
			}

		} catch (PrologInterfaceException e) {
			return UIUtils.createErrorStatus(PDTCorePlugin.getDefault()
					.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
		} catch (CoreException e) {
			return e.getStatus();
		} catch(Throwable t){
			return new Status(Status.ERROR,PDTCore.PLUGIN_ID,"Exception during build",t);
		}
		finally {
			if (s != null) {
				s.dispose();
			}
		}
		return new Status(IStatus.OK, PDTCore.PLUGIN_ID, "done");
	}

	public void update(PrologInterfaceEvent e) {
		if (monitor == null) {
			return;
		}
		if ("expensive".equals(tag)) {
			Debug.debug(e.getSubject() + " <-- " + e.getEvent());
		}
		if (e.getEvent().equals("done")) {
			monitor.done();
			return;
		}

		CTerm term = CTermUtil.createCTerm(e.getEvent());
		if (!(term instanceof CCompound)) {
			Debug.warning("wunder, wunder: term ist kein Compound:"
					+ e.getEvent());
			Debug.warning("Subject: " + e.getSubject() + " , Event: "
					+ e.getEvent());
			return;
		}
		CCompound event = (CCompound) term;
		CTerm argTerm = event.getArgument(0);
		if (!(argTerm instanceof CInteger)) {
			Debug.warning("wunder, wunder: argterm ist kein Integer:"
					+ CTermUtil.renderTerm(argTerm));
			Debug.warning("Subject: " + e.getSubject() + " , Event: \""
					+ e.getEvent() + "\"");
			return;
		}
		int arg = ((CInteger) argTerm).getIntValue();
		String functor = event.getFunctorValue();
		if (functor.equals("start")) {
			monitor.beginTask("Searching for Problems (" + tag + ")", arg);
			return;
		}
		if (functor.equals("worked")) {
			monitor.worked(arg);
			return;
		}
	}

}

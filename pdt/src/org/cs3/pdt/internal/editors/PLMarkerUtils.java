package org.cs3.pdt.internal.editors;

import java.util.List;
import java.util.Map;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.actions.ConsultActionDelegate;
import org.cs3.pdt.quickfix.PDTMarker;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

public class PLMarkerUtils {

	static int mapSeverity(String severity) {
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

	public static void updateFileMarkers(IFile file ) throws CoreException {
		if( PDTCoreUtils.getPrologProject(file)==null &&
				PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole()!= null){ 
			PrologSession session =null;
			try {
				session = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface().getSession();
				session.queryOnce("activate_warning_and_error_tracing");

				file.deleteMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
				executeConsult(file);
				addMarkers(file);

			}catch(Exception e) {
				Debug.report(e);
			} finally {
				if(session!=null)session.dispose();
			}

		}
	}
	

	private static void executeConsult(IFile file ) {
		ConsultActionDelegate consult = new ConsultActionDelegate();
		consult.setSchedulingRule(file);
		consult.run(null);
	}
	
	private static void addMarkers(final IFile file) throws PrologInterfaceException {
		Job j = new Job("update markers") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				PrologSession session =null;
				try {

					final IDocument doc = PDTCoreUtils.getDocument(file);
					session = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface().getSession();
					Thread.sleep(500); // wait for the prolog messages to complete (TODO: wait until parsing is finished)
					add_markers_for_errors_and_warnings(file, session, doc);
					add_markers_for_smell_detectors(file, monitor, session, doc);

					session.queryOnce("deactivate_warning_and_error_tracing");
				} catch (Exception e) {
					Debug.report(e);
					return Status.CANCEL_STATUS;
				} finally {
					if(session!=null)session.dispose();
				}
				return Status.OK_STATUS;
			}
		};
		j.setRule(file);
		j.schedule();


	}
	
	private static void add_markers_for_errors_and_warnings(
			final IFile file, PrologSession session, final IDocument doc)
			throws PrologInterfaceException, CoreException {
		List<Map<String, Object>> msgs = session.queryAll("pdtplugin:errors_and_warnings(Kind,Line,Length,Message)");
		for (Map<String, Object> msg : msgs) {
			int severity=0;
			try {
				severity = mapSeverity(((String)msg.get("Kind")));
			} catch(IllegalArgumentException e){
				continue;
			}
			
			IMarker marker = file.createMarker(IMarker.PROBLEM);

			marker.setAttribute(IMarker.SEVERITY, severity);

			String msgText = (String)msg.get("Message");
			int line = Integer.parseInt((String)msg.get("Line"))-1;
			int start = 0;
			
			try {
				start = doc.getLineOffset(line);
			} catch (BadLocationException e) {
				Debug.warning("Found no position for marker.");
			}
			
			int end = start +Integer.parseInt((String)msg.get("Length"));
			if(severity==IMarker.SEVERITY_ERROR && msgText.startsWith("Exported procedure ")&& msgText.endsWith(" is not defined\n")){
				start = end= 0;
				line = 0;
			}
			
			MarkerUtilities.setCharStart(marker, start);
			MarkerUtilities.setCharEnd(marker, end);
			MarkerUtilities.setLineNumber(marker, line+1);
			
			marker.setAttribute(IMarker.MESSAGE, msgText);
		}
	}

	
	private static void add_markers_for_smell_detectors(final IFile file,
			IProgressMonitor monitor, PrologSession session, final IDocument doc)
			throws PrologInterfaceException, CoreException {
		monitor.setTaskName("Update Prolog Smells Detectors");

		String query = "smell_marker_pdt(Name, Description, QuickfixDescription, QuickfixAction, '" + file.getRawLocation().toPortableString().toLowerCase() + "', Start, Length)";
		List<Map<String, Object>> msgsSmells = session.queryAll(query);

		if(msgsSmells!=null) {
		for (Map<String, Object> msg : msgsSmells) {
			IMarker marker = file.createMarker(IMarker.PROBLEM);
			marker.setAttribute(IMarker.SEVERITY, IMarker.SEVERITY_WARNING);
			marker.setAttribute(PDTMarker.SMELL_NAME, msg.get("Name").toString());
			marker.setAttribute(PDTMarker.QUICKFIX_DESCRIPTION, msg.get("QuickfixDescription").toString());

			String msgText = (String)msg.get("Description");
			int startPl = Integer.parseInt(msg.get("Start").toString());
			int start =PDTCoreUtils.convertLogicalToPhysicalOffset(doc,startPl);
			int length = Integer.parseInt(msg.get("Length").toString());

			//						marker.setAttribute(IMarker.CHAR_START, start);
			//						marker.setAttribute(IMarker.CHAR_END, (start+length));
			MarkerUtilities.setCharStart(marker, start);
			MarkerUtilities.setCharEnd(marker, start+length);

			marker.setAttribute(PDTMarker.QUICKFIX_ACTION, msg.get("QuickfixAction".toString()));
			marker.setAttribute(IMarker.MESSAGE, msgText);
		}
		}
	}


}

package org.cs3.pdt.core.internal.builder;

import java.io.File;
import java.io.IOException;
import java.util.List;
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
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.text.IDocument;
import org.eclipse.ui.texteditor.MarkerUtilities;

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
		PrologSession session = null;
		try {
			plProject.getProject().deleteMarkers(PDTCore.PROBLEM, true, IResource.DEPTH_INFINITE);
			plProject.getMetaDataEventDispatcher().addPrologInterfaceListener(
					"builder(interprete(_))", this);
			PrologInterface2 pif = ((PrologInterface2) plProject
					.getMetadataPrologInterface());
			AsyncPrologSession s = pif.getAsyncSession();
			s.addBatchListener(new DefaultAsyncPrologSessionListener());
			monitor.beginTask("updating", files.size()+100);
			s.queryOnce("update_markers", "pdt_with_targets([problems],true)");
			s.join();
			s.dispose();
			session = pif.getSession();
			List<Map> l = session.queryAll("pdt_problem(File,Start,End,Severity,Msg)");
			for (Map m : l) {
				String filename = (String) m.get("File");
				int start = Integer.parseInt(((String)m.get("Start")));
				int end = Integer.parseInt(((String)m.get("End")));
				String severity= (String) m.get("Severity");
				String message = (String) m.get("Msg");
				addMarker(filename,start,end,severity,message);
				monitor.worked(100/l.size());
			}
			IMarker[] findMarkers = plProject.getProject().findMarkers(IMarker.PROBLEM, true, IResource.DEPTH_INFINITE);
			monitor.done();
			monitor = null;
			plProject.getMetaDataEventDispatcher()
					.removePrologInterfaceListener("builder(interprete(_))",
							this);
		} catch (PrologInterfaceException e) {
			UIUtils.createErrorStatus(PDTCorePlugin.getDefault()
					.getErrorMessageProvider(), e, PDTCore.ERR_PIF);
		} catch (CoreException e) {
			UIUtils.createErrorStatus(PDTCorePlugin.getDefault()
					.getErrorMessageProvider(), e, PDTCore.ERR_UNKNOWN);
		}finally{
			if(session!=null){
				session.dispose();
			}
		}
		return new Status(IStatus.OK, PDTCore.PLUGIN_ID, "done");
	}

	private void addMarker(String filename, int start, int end,
			String severity, String message) throws CoreException {
		IFile file=null;
		try {
			
			file = PDTCoreUtils.findFileForLocation(filename);
			
		}  catch(IllegalArgumentException iae){
			//ignore files that are not in the workspace.
			//Debug.report(iae);
			;
		}catch (IOException e) {
			Debug.rethrow(e);
		}
		if(file==null){
			return;
		}
		IMarker marker = file.createMarker(PDTCore.PROBLEM);
		IDocument doc = PDTCoreUtils.getDocument(file);
		start = PDTCoreUtils.convertCharacterOffset(doc, start);
		end = Math.max(start + 1, PDTCoreUtils.convertCharacterOffset(doc, end));
		end = Math.min(doc.getLength(),end);
		
		MarkerUtilities.setCharStart(marker, start);
		MarkerUtilities.setCharEnd(marker, end);
		
		marker.setAttribute(IMarker.SEVERITY, mapSeverity(severity));
		marker.setAttribute(IMarker.MESSAGE, message);
	}

	private int mapSeverity(String severity) {
		if("error".equals(severity)){
			return IMarker.SEVERITY_ERROR;
		}
		if("warning".equals(severity)){
			return IMarker.SEVERITY_WARNING;
		}
		if("info".equals(severity)){
			return IMarker.SEVERITY_INFO;
		}
		
		throw new IllegalArgumentException("cannot map severity constant: "+severity);
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

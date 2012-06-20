package org.cs3.pdt.internal.editors;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ConsultListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public class EditorConsultListener implements ConsultListener {

	@Override
	public void beforeConsult(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		activateWarningAndErrorTracing(pif, monitor);
	}

	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		activateWarningAndErrorTracing(pif, monitor);
	}

	private void activateWarningAndErrorTracing(PrologInterface pif, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("activate warning and error tracing", 1);
		pif.queryOnce("activate_warning_and_error_tracing");
		monitor.done();
	}
	
	@Override
	public void afterConsult(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		PLMarkerUtils.addMarkers(pif, monitor);
	}

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		PLMarkerUtils.addMarkers(pif, monitor);
	}

}

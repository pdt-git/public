package org.cs3.pdt.internal.editors;

import java.util.List;

import org.cs3.pdt.internal.views.lightweightOutline.NonNaturePrologOutline;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ConsultListener;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class EditorConsultListener implements ConsultListener {

	@Override
	public void beforeConsult(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		monitor.done();
	}

	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		monitor.done();
	}

	@Override
	public void afterConsult(PrologInterface pif, IFile file, IProgressMonitor monitor) throws PrologInterfaceException {
		PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				IEditorPart editor = UIUtils.getActiveEditor();

				if ((editor == null) || !(editor instanceof PLEditor)) {
					return;
				}
				PLEditor pleditor = (PLEditor)editor;
				ContentOutlinePage outlinePage = pleditor.getOutlinePage();
				if ((outlinePage != null) && (outlinePage instanceof NonNaturePrologOutline)){
					NonNaturePrologOutline prologOutlinePage = ((NonNaturePrologOutline)outlinePage);
					prologOutlinePage.setInput(pleditor.getEditorInput());
				}
			}
		});
		PLMarkerUtils.addMarkers(pif, monitor);
	}

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		PLMarkerUtils.addMarkers(pif, monitor);
	}
	
}

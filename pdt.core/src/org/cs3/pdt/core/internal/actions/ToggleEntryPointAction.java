package org.cs3.pdt.core.internal.actions;

import java.util.Iterator;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class ToggleEntryPointAction implements IObjectActionDelegate {

	private IFile file;

	private IProject project;

	private IPrologProject plProject;

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {


	}

	public void run(IAction action) {
		Set<IFile> entries;
		try {
			entries = plProject.getExistingEntryPoints();

			if(action.isChecked()){
				entries.add(file);
			}else{
				entries.remove(file);
			}
			
			
			String sep = System.getProperty("path.separator");
			StringBuffer sb = new StringBuffer();
			for (Iterator<IFile> it = entries.iterator(); it.hasNext();) {
				IFile entry =  it.next();

				
				IPath path = entry.getProjectRelativePath();
				String string = path.toPortableString();

				
				sb.append(string);
				if (it.hasNext()) {
					sb.append(sep);
				}
			}
			
			plProject.setPreferenceValue(PDTCore.PROP_ENTRY_POINTS, sb.toString());
			updateAction(action);
		} catch (CoreException e) {
//			UIUtils.logAndDisplayError(PDTCorePlugin.getDefault()
//					.getErrorMessageProvider(), UIUtils.getDisplay()
//					.getActiveShell(), PDTCore.ERR_UNKNOWN,
//					PDTCore.CX_TOGGLE_ENTRY_POINT, e);
		}
	}

	public void selectionChanged(IAction action, ISelection selection) {
		file = null;
		try {
			if (selection instanceof IStructuredSelection) {
				Object obj = ((IStructuredSelection) selection)
						.getFirstElement();
				if (obj instanceof IFile) {
					file = (IFile) obj;
				} else if (obj instanceof IAdaptable) {
					IAdaptable a = (IAdaptable) obj;
					IFile r = (IFile) a.getAdapter(IFile.class);
					if (r != null
							&& (IResource.FILE == r.getType() )) {
						file = (IFile) r;
					}
				}
			}
			if (file != null) {
				project = file.getProject();
			} else {
				project = null;
			}
			if (project != null&&project.isOpen()) {

				if (project.hasNature(PDTCore.NATURE_ID)) {
					plProject = (IPrologProject) project
							.getNature(PDTCore.NATURE_ID);

				} else {
					plProject = null;
				}

			} else {
				plProject = null;
			}
			updateAction(action);
		} catch (CoreException e) {
//			UIUtils.logAndDisplayError(PDTCorePlugin.getDefault()
//					.getErrorMessageProvider(), UIUtils.getDisplay()
//					.getActiveShell(), PDTCore.ERR_UNKNOWN,
//					PDTCore.CX_TOGGLE_ENTRY_POINT, e);
		}
	}

	private void updateAction(IAction action) throws CoreException {
		if (plProject == null) {
			action.setEnabled(false);
			action.setChecked(false);
		} else {
			action.setEnabled(true);
			action.setChecked(plProject.getExistingEntryPoints()
					.contains(file));
		}

	}

}

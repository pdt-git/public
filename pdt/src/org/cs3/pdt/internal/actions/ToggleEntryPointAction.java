package org.cs3.pdt.internal.actions;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.cs3.pdt.PDTPlugin;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionDelegate;

public class ToggleEntryPointAction implements IActionDelegate {

	public static final QualifiedName KEY = new QualifiedName("pdt", "entry.point");
	
	public ToggleEntryPointAction() {
	}

	@Override
	public void run(IAction action) {
		if (selectedFiles != null) {
			
			
			if (isSelectionChecked()) {
				for (IFile file : selectedFiles) {
					setEntryPoint(file, false);
				}
			} else {
				for (IFile file : selectedFiles) {
					setEntryPoint(file, true);
				}
			}
			PDTPlugin.getDefault().notifyDecorators();
			
//			for (IFile file : selectedFiles) {
//				if (isEntryPoint(file)) {
//					setEntryPoint(file, false);
//				} else {
//					setEntryPoint(file, true);
//				}
//				
//				PDTPlugin.getDefault().notifyDecorators();
//			}
		}
		
	}

	Set<IFile> selectedFiles;
	
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		selectedFiles = new HashSet<IFile>();
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection selections = (IStructuredSelection) selection;

			for (Iterator<?> iter = selections.iterator(); iter.hasNext();) {
				Object obj = iter.next();
				if (obj instanceof IFile) {
					selectedFiles.add((IFile) obj);
				}
			}
			action.setChecked( isSelectionChecked() );
		}
		
	}

	private boolean isSelectionChecked() {
		boolean result = false;
		if (selectedFiles != null && selectedFiles.size() > 0) {
			result = true;
			for (IFile f : selectedFiles) {
				if (!isEntryPoint(f)) {
					result = false;
					break;
				}
			}
		}
		return result;
	}
	
	private boolean isEntryPoint(IFile file) {
		try {
			String prop = file.getPersistentProperty(KEY);
			if (prop != null && prop.equalsIgnoreCase("true")) {
				return true;
			}
		} catch (CoreException e) {}

		return false;
	}
	
	private void setEntryPoint(IFile file, boolean b) {
		try {
			file.setPersistentProperty(KEY, Boolean.toString(b));
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}


}

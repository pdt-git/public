/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.actions;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IActionDelegate;

public class ToggleEntryPointAction implements IActionDelegate {

	public ToggleEntryPointAction() {
	}

	@Override
	public void run(IAction action) {
		if (selectedFiles != null) {
			
			PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();
			if (isSelectionChecked()) {
				for (IFile file : selectedFiles) {
					setEntryPoint(file, false, pif);
				}
			} else {
				for (IFile file : selectedFiles) {
					setEntryPoint(file, true, pif);
				}
			}
			PDTPlugin.getDefault().notifyDecorators();
			
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
			String prop = file.getPersistentProperty(PDTCommonPlugin.ENTRY_POINT_KEY);
			if (prop != null && prop.equalsIgnoreCase("true")) {
				return true;
			}
		} catch (CoreException e) {}

		return false;
	}
	
	private void setEntryPoint(IFile file, boolean b, PrologInterface pif) {
		try {
			file.setPersistentProperty(PDTCommonPlugin.ENTRY_POINT_KEY, Boolean.toString(b));
			if (b) {
				PDTCommonPlugin.getDefault().addEntryPoint(file);
			} else {
				PDTCommonPlugin.getDefault().removeEntryPoint(file);
			}
		} catch (CoreException e) {
			e.printStackTrace();
		}
		
		if (pif != null) {
			try {
				String prologFileName = Util.prologFileName(file.getLocation().toFile().getCanonicalFile());
				
				if (b) {
					pif.queryOnce(bT(PDTCommonPredicates.ADD_ENTRY_POINT, Util.quoteAtom(prologFileName)));
				} else {
					pif.queryOnce(bT(PDTCommonPredicates.REMOVE_ENTRY_POINTS, Util.quoteAtom(prologFileName)));
				}
			} catch (IOException e) {
				Debug.report(e);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
			
			
		}
	}


}


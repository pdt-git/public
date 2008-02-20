package org.cs3.pdt.internal.refactorings;

import java.util.Map;

import org.cs3.pdt.transform.PrologRefactoringDescriptor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;

public class RenameFileDescriptor extends PrologRefactoringDescriptor {
	

	@Override
	public String getParametersTerm(Map<String, String> parameterValues) {

		return parameterValues.get("name");
	}

	@Override
	public String getSelectionTerm(ISelection selection,
			IWorkbenchPartReference activePart) {

		IFile file = selectedFile(selection, activePart);
		if(file==null){
			return null;
		}
		return Util.prologFileName(file.getLocation().toFile());
	}

	private IFile selectedFile(ISelection selection, IWorkbenchPartReference activePart) {
		if(activePart.getId().equals("org.cs3.pdt.internal.editors.PLEditor")){
			return UIUtils.getFileInActiveEditor();
		}
		IFile file=null;
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
		return file;
	}

	@Override
	public Option[] getParameters(ISelection selection,
			IWorkbenchPartReference activePart) {
		IFile file = selectedFile(selection, activePart);
		return new Option[] {

				new SimpleOption("path", "CurrentPath", "", Option.FILE, Util
						.prologFileName(file.getLocation().toFile())) {
					@Override
					public boolean isVisible() {
						return false;
					}

				},
				new SimpleOption("name", "New Name", "the new file name",
						Option.STRING, file.getName()) };
	}
}

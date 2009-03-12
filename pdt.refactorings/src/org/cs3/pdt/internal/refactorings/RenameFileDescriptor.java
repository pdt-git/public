package org.cs3.pdt.internal.refactorings;

import java.util.Map;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.transform.PrologRefactoringDescriptor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbenchPart;

public class RenameFileDescriptor extends PrologRefactoringDescriptor{
	
	
	public String getParametersTerm(Map<String, String> parameterValues) {

		String newName = parameterValues.containsKey("NewName") ? "'"+parameterValues.get("NewName")+"'" : "NewName";
		String file = parameterValues.containsKey("File") ? parameterValues.get("File") : "File";
		return "params("+file+","+newName+")";
	}

	
	public String getSelectionTerm(ISelection selection,
			IWorkbenchPart activePart) {

		IFile file = selectedFile(selection, activePart);
		if(file==null){
			return null;
		}
		return "file('"+Util.prologFileName(file.getLocation().toFile())+"')";
	}

	private IFile selectedFile(ISelection selection, IWorkbenchPart activePart) {
		if(activePart.getSite().getId().equals("org.cs3.pdt.internal.editors.PLEditor")){
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

	public Option[] getParameters(ISelection selection,
			IWorkbenchPart activePart) {
		
		return new Option[] {

				new SimpleOption("File", "Id of the file to rename.", "", Option.NUMBER, null) {
					
					public boolean isVisible() {
						return false;
					}
					
					public boolean isEditable() {
						return false;
					}
				},
				new SimpleOption("NewName", "New Name", "The new file name.",
						Option.STRING, null) };
	}
	
	
	public PrologInterface getPrologInterface(ISelection selection,IWorkbenchPart activePart) throws CoreException{
		IFile file = selectedFile(selection, activePart);
		return PDTCoreUtils.getPrologProject(file).getMetadataPrologInterface();
		
	}
}

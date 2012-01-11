package org.cs3.pdt.console.internal.loadfile;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

public class GenerateLoadFileWizard extends Wizard implements INewWizard {

	private List<String> consultedFiles;
	
	private IStructuredSelection selection;
	private GenerateLoadFilePage newFileWizardPage;

	public GenerateLoadFileWizard(List<String> consultedFiles) {
		setWindowTitle("Generate prolog load file");
		this.consultedFiles = consultedFiles;
	} 

    @Override    
    public void addPages() {
    	if (selection == null) {
    		selection = new StructuredSelection();
    	}
    	newFileWizardPage = new GenerateLoadFilePage(selection);
    	addPage(newFileWizardPage);
    }
    
    @Override
    public boolean performFinish() {
    	IFile file = newFileWizardPage.createNewFile();
    	
    	if (file != null) {
    		writeFileContent(file);

    		return true;
    	}
    	else
    		return false;
    	}

	public void writeFileContent(IFile file) {
		try {
			IPath location = file.getLocation();
			String locationString = location.toString().toLowerCase();
			locationString = locationString.substring(0, locationString.lastIndexOf("/") + 1);
			
			StringBuffer buf = new StringBuffer();
			for (String fileName : consultedFiles) {
				if (fileName.startsWith("'" + locationString)) {
					fileName = "'" + fileName.substring(locationString.length() + 1);
				}
				buf.append(":- consult(" + fileName + ").\n");
			}
			String content = buf.toString();
			
			file.setContents(new ByteArrayInputStream(content.getBytes()), IResource.FORCE, null);
		} catch (CoreException e) {
			e.printStackTrace();
		}
	}
    
    @Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
//    	this.workbench = workbench;
    	this.selection = selection;
    }


    public class GenerateLoadFilePage extends WizardNewFileCreationPage {

    	public GenerateLoadFilePage(IStructuredSelection selection) {
    		super("GenerateLoadFilePage", selection);  
    		setTitle("Load File");
    		setDescription("Creates a new load file");
    		setFileExtension("pl");
    		setFileName("load.pl");
    	}

    }

}


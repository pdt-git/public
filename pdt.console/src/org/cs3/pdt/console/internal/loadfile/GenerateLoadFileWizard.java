package org.cs3.pdt.console.internal.loadfile;

import java.io.ByteArrayInputStream;
import java.util.List;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
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
    		if (newFileWizardPage.isEntryPoint()) {
    			// TODO: set entry point
    		}

    		return true;
    	}
    	else
    		return false;
    	}

	public void writeFileContent(IFile file) {
		try {
			IPath loadFileLocation = file.getLocation();
			IPath loadFileFolder = new Path(loadFileLocation.toString().toLowerCase()).removeLastSegments(1);
			IPath projectLocation = file.getProject().getLocation();
			IPath projectFolder = new Path(projectLocation.toString().toLowerCase());
			
			StringBuffer buf = new StringBuffer();
			buf.append(":- dynamic user:file_search_path/2.\n");
			buf.append(":- multifile user:file_search_path/2.\n");
			buf.append(":- prolog_load_context(directory, Dir), asserta(user:file_search_path(load_file_dir, Dir)).\n");
			IPath projectFromLoadFile = projectFolder.makeRelativeTo(loadFileFolder);
			String projectAlias = Util.quoteAtom(file.getProject().getName());
			buf.append("file_search_path(" + projectAlias + ", load_file_dir('" + projectFromLoadFile.toString() + "')).\n\n");
			for (String fileName : consultedFiles) {
				String fileNameWithoutQuotes = Util.unquoteAtom(fileName);
				IPath path = new Path(fileNameWithoutQuotes);
				IPath relPath = path.makeRelativeTo(projectFolder);
				buf.append(":- consult(" + projectAlias + "('" + relPath.toString().toLowerCase() + "')).\n");
			}
			String content = buf.toString();
			
			file.setContents(new ByteArrayInputStream(content.getBytes()), IResource.FORCE, null);
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
    
    @Override
	public void init(IWorkbench workbench, IStructuredSelection selection) {
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
    	
    	Button entryPointCheckbox;
    	
    	 @Override
		public void createControl(Composite parent) {
    	      // inherit default container and name specification widgets
    	      super.createControl(parent);
    	      Composite composite = (Composite)getControl();
    	      
    	      entryPointCheckbox = new Button(composite,SWT.CHECK);
    	      entryPointCheckbox.setText("Mark file as entry point");
    	      entryPointCheckbox.setSelection(true);
    	 }
    	 
    	 public boolean isEntryPoint() {
    		 return entryPointCheckbox.getSelection();
    	 }

    }

}


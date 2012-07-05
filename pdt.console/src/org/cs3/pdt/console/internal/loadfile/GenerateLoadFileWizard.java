package org.cs3.pdt.console.internal.loadfile;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.QualifiedName;
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

	private static final QualifiedName KEY = new QualifiedName("pdt", "entry.point");

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
    			addEntryPoint(file);
    		}

    		return true;
    	}
    	else
    		return false;
    	}

	public void addEntryPoint(IFile file) {
		try {
			file.setPersistentProperty(KEY, "true");
			
			PrologConsolePlugin consolePlugin = PrologConsolePlugin.getDefault();

			consolePlugin.addEntryPoint(file);
			
			PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface();

			if (pif != null) {
				try {
					String prologFileName = Util.prologFileName(file.getLocation().toFile().getCanonicalFile());

					pif.queryOnce(bT("add_entry_point", "'" + prologFileName + "'"));
				} catch (IOException e) {
					Debug.report(e);
				} catch (PrologInterfaceException e) {
					Debug.report(e);
				}
			}
		} catch (CoreException e) {
			Debug.report(e);
		}
	}

	public void writeFileContent(IFile file) {
		try {
			IPath projectLocation = file.getProject().getLocation();
			IPath projectFolder = new Path(projectLocation.toString().toLowerCase());
			
			StringBuffer buf = new StringBuffer();
			buf.append(":- dynamic user:file_search_path/2.\n");
			buf.append(":- multifile user:file_search_path/2.\n");
			String projectAlias = Util.quoteAtom(file.getProject().getName());
			buf.append("user:file_search_path(" + projectAlias + ", '" + projectFolder + "').\n\n");
			for (String fileName : consultedFiles) {
				String fileNameWithoutQuotes = Util.unquoteAtom(fileName);
				IPath path = new Path(fileNameWithoutQuotes);
				IPath relPath = path.makeRelativeTo(projectFolder);
				if (path.equals(relPath)) {
					buf.append(":- consult('" + relPath.toString().toLowerCase() + "').\n");
				} else {
					buf.append(":- consult(" + projectAlias + "('" + relPath.toString().toLowerCase() + "')).\n");
				}
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


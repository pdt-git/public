package org.cs3.pdt.internal.wizards;


import java.io.ByteArrayInputStream;

import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.PlatformObject;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.part.FileEditorInput;

public class NewModuleCreationWizardPage extends WizardPage  {

	/**
	 * copied from: org.eclipse.jdt.ui.wizards.ImportsManager:
	 */
	
	
	private final static String PAGE_NAME= "NewModuleCreationWizardPage"; //$NON-NLS-1$
	private Text moduleName;
	private Text folderName;

	
	/**
	 * Create a new <code>NewAnnotationWizardPage</code>
	 */	
	public NewModuleCreationWizardPage() {
		super(PAGE_NAME);
		
		setTitle("New Prolog Module"); 
		setDescription("New Module");
		
		
	}
	

	/*
	 * Called from createType to construct the source for this type
	 */		
	
	
//	private String constructSimpleTypeStub() {
//		StringBuffer buf= new StringBuffer(":- module"); //$NON-NLS-1$
////		buf.append(getTypeName());
//		buf.append(", [\n   ]).");
//		return buf.toString();
//	}

	

	/*
	 * @see WizardPage#createControl
	 */
	public void createControl(Composite parent) {
		initializeDialogUnits(parent);
		
		Composite composite= new Composite(parent, SWT.NONE);
		
		int nColumns= 2;
		
		GridLayout layout= new GridLayout();
		layout.numColumns= nColumns;		
		composite.setLayout(layout);
		
		GridData gridData = new GridData();
	    gridData.grabExcessHorizontalSpace = true;
	    gridData.horizontalAlignment = GridData.FILL;
	    
		Label label = new Label(composite,SWT.NONE);
		label.setText("Module Name: ");

		moduleName = new Text(composite,SWT.BORDER);
		moduleName.setLayoutData(gridData);
		
		Label folderLabel = new Label(composite,SWT.NONE);
		folderLabel.setText("Folder: ");

		folderName = new Text(composite,SWT.BORDER);
		folderName.setLayoutData(gridData);

		ISelection selection = UIUtils.getActivePage().getSelection();
		
		// Navigator selection:
		if(selection instanceof TreeSelection){
			IResource resource= (IResource)((PlatformObject)((TreeSelection) selection).getFirstElement()).getAdapter(IResource.class);
			if(resource !=null){
				IPath fullPath = resource.getFullPath();
				if(resource instanceof IFile){
					folderName.setText(fullPath.removeLastSegments(1).toPortableString());
				} else {
					folderName.setText(fullPath.toPortableString());
				}
			}
//			String path = 
//			folderName.setText(path);
		} else {

			IEditorPart editor = UIUtils.getActivePage().getActiveEditor();
			if(editor!=null && editor.getEditorInput()instanceof FileEditorInput){
				IPath path = ((FileEditorInput)editor.getEditorInput()).getFile().getFullPath().removeLastSegments(1);
				folderName.setText(path.toPortableString());
			}
			
		}
		
		setControl(composite);
		
		Dialog.applyDialogFont(composite);
	}


	/*
	 * @see WizardPage#becomesVisible
	 */
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
			moduleName.setFocus();
		}
	}

	

	public IFile createModule(IProgressMonitor monitor) throws CoreException {
		IFile file =null;
		try {
			String path= folderName.getText();
			if(!path.endsWith("/")){
				path +="/";
			}
			path += moduleName.getText();
			IPath ipath = new Path(path).addFileExtension("pl");
			file = ResourcesPlugin.getWorkspace().getRoot().getFile(ipath);
			if(file.exists()){
				MessageBox box = new MessageBox(UIUtils.getActiveShell());
				box.setMessage("File already exists in workspace: " + ipath.toOSString());
				box.setText("Cannot create module");
				box.open();
				throw new CoreException(Status.CANCEL_STATUS);
			}
			
			String content =":- module(" + moduleName.getText() + ",[\n      ]).\n";
			file.create(new ByteArrayInputStream(content.getBytes()), true, null);
			
		} finally {
			if(monitor!=null)
				monitor.done();
		}
		
		return file;
	}
	

}

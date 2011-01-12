package org.cs3.pdt.internal.wizards;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class NewModuleCreationWizardPage extends WizardPage  {

	/**
	 * copied from: org.eclipse.jdt.ui.wizards.ImportsManager:
	 */
	
	
	private final static String PAGE_NAME= "NewModuleCreationWizardPage"; //$NON-NLS-1$

	
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
	
	
	private String constructSimpleTypeStub() {
		StringBuffer buf= new StringBuffer(":- module"); //$NON-NLS-1$
//		buf.append(getTypeName());
		buf.append(", [\n   ]).");
		return buf.toString();
	}

	

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
		
		Label label = new Label(composite,SWT.NONE);
		label.setText("module name");

		Text text = new Text(composite,SWT.NONE);

		setControl(composite);
		
		Dialog.applyDialogFont(composite);
	}


	/*
	 * @see WizardPage#becomesVisible
	 */
	public void setVisible(boolean visible) {
		super.setVisible(visible);
		if (visible) {
//			setFocus();
		}
	}


	public IResource getModifiedResource() {
		return null;
	}


	public void createModule(IProgressMonitor monitor) {
		// TODO Auto-generated method stub
		
	}
	

}

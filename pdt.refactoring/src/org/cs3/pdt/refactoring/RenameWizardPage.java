package org.cs3.pdt.refactoring;

import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

public class RenameWizardPage  extends UserInputWizardPage {

	public final static String NAME = "Rename Prolog Element";
	private Text fNameField;

	
	public RenameWizardPage() {
		super(NAME);
	}

	@Override
	public void createControl(Composite parent) {
		Composite composite= new Composite(parent, SWT.NONE);
		composite.setLayout(new GridLayout(2, false));
		composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		composite.setFont(parent.getFont());

		Label label= new Label(composite, SWT.NONE);
		label.setText("Enter new name");
		label.setLayoutData(new GridData());

		fNameField= new Text(composite, SWT.BORDER);
		fNameField.setFont(composite.getFont());
		fNameField.setLayoutData(new GridData(GridData.FILL, GridData.BEGINNING, true, false));
		fNameField.addModifyListener(new ModifyListener() {
			public void modifyText(ModifyEvent e) {
				validatePage();
			}
		});
		
		

		
		//fNameField.selectAll();
		setPageComplete(false);
		setControl(composite);		
	}
	
	private void validatePage() {
		String text= fNameField.getText();
		boolean status;
		if(text == null || text.equalsIgnoreCase(""))
			status = false;
		else
			status = true;
		((PDTRefactoringWizard)getWizard()).setNewText(text);
		setPageComplete(status);
	}
	
	public String getNewName() {
		return fNameField.getText();
	}

}

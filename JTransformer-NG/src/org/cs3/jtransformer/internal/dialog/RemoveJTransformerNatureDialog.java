package org.cs3.jtransformer.internal.dialog;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

public class RemoveJTransformerNatureDialog {

	private Shell sShell = null;  //  @jve:decl-index=0:visual-constraint="51,39"
	private Label question = null;
	private Button removeOutputProjectReference = null;
	private Button deleteOutputProject = null;
	private Button OK = null;
	private Button cancel = null;
	private Shell shell;
	protected boolean cancelDialog = false;
	private boolean deleteProject = true;
	private boolean removeReference = true;
	private String projectNamePresentedInQuestion;
	/**
	 * @param args
	 */
	public static void main(String[] args) {

		Display display = Display.getDefault();
		Shell shell = new Shell(display);
		RemoveJTransformerNatureDialog thisClass = new RemoveJTransformerNatureDialog(shell, "project name");
		thisClass.open();
		display.dispose();
	}

	public RemoveJTransformerNatureDialog(Shell shell,String projectNamePresentedInQuestion) {
		this.shell = shell;
		this.projectNamePresentedInQuestion = projectNamePresentedInQuestion;
		createSShell();
	}

	/**
	 * This method initializes sShell
	 */
	private void createSShell() {
		GridData gridData = new GridData();
		gridData.grabExcessHorizontalSpace = true;
		gridData.horizontalSpan = 2;
		GridLayout gridLayout = new GridLayout();
		gridLayout.numColumns = 2;
		sShell = new Shell();
		sShell.setSize(new Point(380, 122));
		sShell.setText("JTransformer");
		sShell.setLayout(gridLayout);
		sShell.setSize(new Point(489, 126));
		question = new Label(sShell, SWT.NONE);
		question.setText("Do you want to remove the JTransformer Nature from " + projectNamePresentedInQuestion+ "?");
		question.setLayoutData(gridData);
		removeOutputProjectReference = new Button(sShell, SWT.CHECK);
		removeOutputProjectReference.setText("remove reference to output Project");
		removeOutputProjectReference.setEnabled(false);
		removeOutputProjectReference.setSelection(true);
		removeOutputProjectReference
				.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
					public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
						removeReference = removeOutputProjectReference.getSelection();

					}
				});
		Label filler1 = new Label(sShell, SWT.NONE);
		deleteOutputProject = new Button(sShell, SWT.CHECK);
		deleteOutputProject.setText("delete output project");
		deleteOutputProject.setSelection(true);
		deleteOutputProject
				.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
					public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
						
						deleteProject = deleteOutputProject.getSelection();
						if(deleteOutputProject.getSelection()){
							removeOutputProjectReference.setSelection(true);
							removeReference = true;
							removeOutputProjectReference.setEnabled(false);
						} else {
							removeOutputProjectReference.setEnabled(true);
						}
					}
				});
		Label filler = new Label(sShell, SWT.NONE);
		OK = new Button(sShell, SWT.NONE);
		OK.setText("OK");
		OK.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				doExit();
			}
		});
		cancel = new Button(sShell, SWT.NONE);
		cancel.setText("Cancel");
		cancel.addSelectionListener(new org.eclipse.swt.events.SelectionAdapter() {
			public void widgetSelected(org.eclipse.swt.events.SelectionEvent e) {
				cancelDialog  = true;
				doExit();
			}
		});
	}

	/**
	 * This method initializes sShell
	 */
	public boolean open() {
		
		sShell.open();

		while (!sShell.isDisposed()) {
			if (!shell.getDisplay().readAndDispatch())
				shell.getDisplay().sleep();
		}
		return !cancelDialog;
	}

	private void doExit() {
		sShell.close();
		sShell.dispose();
	}
	
	public boolean isDeleteOutputProject() {
		return deleteProject;
	}

	public boolean isRemoveOutputProjectReference() {
		return removeReference;
	}

}

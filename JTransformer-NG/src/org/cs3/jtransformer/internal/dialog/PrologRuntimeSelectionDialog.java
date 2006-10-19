package org.cs3.jtransformer.internal.dialog;
/*******************************************************************************
 * Copyright (c) 2003, 2005 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * 
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.eclipse.jface.viewers.CellEditor.LayoutData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.Text;



public class PrologRuntimeSelectionDialog {

	private static final String DEFAULT_TITLE = "Add JTransformer Nature";

	private String selected = "";  //  @jve:decl-index=0:

	private Shell dialogShell = null;  //  @jve:decl-index=0:visual-constraint="67,7"

	private static String lastRuntime;

	private boolean isClosing = false;

	//private String selected = null;

	private Table availablePrologRuntimes = null;

	private Composite buttons = null;

	private Button okButton = null;

	private Button cancelButton = null;

	private Composite radioButtons = null;
	
	private Button radioDefault = null;
	private Button radioNamed = null;
	private Button radioSelect = null;
	private Text name = null;
	
	private Shell shell;

	private List runtimes;

	private String projectRuntime;

	private List subscriptions;

	private Button addReferencedProjects = null;

	/**
	 * Transfer variable for the state of the addReferencedProjects check box.
	 */
	private boolean includeReferencedProjects = false;
	
	private void updateSelection(String newSelection) {
		updateSelection(newSelection,true);
	}
	
	public PrologRuntimeSelectionDialog(Shell shell, List runtimes,List subscriptions, String projectRuntime) {
		this.shell = shell;
		this.runtimes = runtimes;
		this.subscriptions = subscriptions;
		this.projectRuntime = projectRuntime;
//		this.selected = projectRuntime;
		init();
	}
	
	/**
	 * This method initializes buttons	
	 *
	 */
	
	
	private void createButtons() {

		
		radioDefault = new Button(radioButtons, SWT.RADIO);
		radioDefault.setText("use project name as factbase name");
		radioDefault.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}
			public void widgetSelected(SelectionEvent e) {
				updateSelection(projectRuntime);		
			}
			
		});
		
		Label filler1 = new Label(radioButtons, SWT.NONE);
		//defaultRuntimeLabel.setText(projectRuntime);
		radioNamed = new Button(radioButtons, SWT.RADIO);
		radioNamed.setText("explicitly name factbase");
		name = new Text(radioButtons, SWT.BORDER);
		name.addKeyListener(new KeyListener() {

			public void keyReleased(KeyEvent e) {
				if(e.character == '\r' || e.character == '\n') {
					doExit();
				}else {
					updateSelection(name.getText(),false);
				}
			}

			public void keyPressed(KeyEvent e) {
			}
			
		});
				
		GridData data = new GridData();
		data.widthHint = 230;
		data.grabExcessHorizontalSpace = true;
		name.setLayoutData(data);
		//name.setSize(220, 16);
		name.setEnabled(false);
		
		radioSelect = new Button(radioButtons, SWT.RADIO);
		radioSelect.setText("select existing factbase");
		Label filler3 = new Label(radioButtons, SWT.NONE);

		radioDefault.addSelectionListener(new ToggleRadioButtonSelection(radioButtons,radioDefault));
		radioNamed.addSelectionListener(new ToggleRadioButtonSelection(radioButtons,radioNamed));
		radioSelect.addSelectionListener(new ToggleRadioButtonSelection(radioButtons,radioSelect));
				
		okButton = new Button(buttons, SWT.NONE);
		okButton.setText("     OK     ");
		
		okButton.setSelection(true);
		cancelButton = new Button(buttons, SWT.NONE);
		cancelButton.setText("  Cancel  ");

		FormData formDataOK = new FormData();
		formDataOK.left = new FormAttachment(3, 40,100);

		FormData formDataJoin = new FormData();
		formDataJoin.left = new FormAttachment(4, 10,10);
		FormData formDataCancel = new FormData();
		formDataCancel.right = new FormAttachment(6, 10,10);
		okButton.setLayoutData(formDataOK);
		cancelButton.setLayoutData(formDataCancel);

		
		okButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}
			
			public void widgetSelected(SelectionEvent e) {
				//updateSelection(projectRuntime);
				doExit();
			}
			
		});
				
		cancelButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			public void widgetSelected(SelectionEvent e) {
				cancelled();
			}

			
		});
	}
	protected void updateSelection(String newSelection, boolean updateNameTextField) {
		selected = newSelection;
		dialogShell.setText(DEFAULT_TITLE + " to project " + projectRuntime );//" - factbase name: \"" + newSelection+ "\"");
		if(updateNameTextField) {
			name.setText(newSelection);
		}
	}


	private boolean cancelled() {
		updateSelection("");		
		return doExit();
	}

	private void initSelection() {
		
		if(lastRuntime != null) {
			updateSelection(lastRuntime);
		// selected runtime
			if(runtimes.size() > 0 && runtimes.indexOf(lastRuntime) > 0) {
				availablePrologRuntimes.select(runtimes.indexOf(lastRuntime));
				radioSelect.setSelection(true);
				availablePrologRuntimes.setEnabled(true);

		// named runtime				
			} else {
				name.setEnabled(true);
				availablePrologRuntimes.select(0);
				radioNamed.setSelection(true);
			}	
		// default runtime				
		} else {		
			updateSelection(projectRuntime);
			radioDefault.setSelection(true);
		}
		//name.setla
	}

	public static void main(String[] args) {
		/* Before this is run, be sure to set up the following in the launch configuration 
		 * (Arguments->VM Arguments) for the correct SWT library path. 
		 * The following is a windows example:
		 * -Djava.library.path="installation_directory\plugins\org.eclipse.swt.win32_3.0.0\os\win32\x86"
		 */
		org.eclipse.swt.widgets.Display display = org.eclipse.swt.widgets.Display
				.getDefault();
		Shell shell = new Shell(display);
		List list = new ArrayList();
		list.add("runtime1"); list.add("runtime2"); list.add("runtime3"); list.add("runtime4"); list.add("runtime5"); 
		list.add("runtime2"); list.add("runtime1"); list.add("runtime2");
		
		List subs = new ArrayList();
		subs.add("runtime1"); subs.add("runtime2"); subs.add("runtime3"); subs.add("runtime4"); subs.add("runtime5"); 
		subs.add("runtime2"); subs.add("runtime1"); subs.add("runtime2");
		PrologRuntimeSelectionDialog thisClass = new PrologRuntimeSelectionDialog(
				shell,
				list,subs, "new.runtime");
		//lastRuntime = "runtime3";
		String selected = thisClass.open();
		
		System.out.println(selected);

		display.dispose();
	}

	/**
	 * This method initializes sShell
	 */
	public String open() {
		
		dialogShell.open();

		Iterator iter2 = subscriptions.iterator();
		for (Iterator iter = runtimes.iterator(); iter.hasNext();) {
			String runtime = (String) iter.next();
			
			TableItem item = new TableItem(availablePrologRuntimes, 
					SWT.SINGLE);
			
			item.setText(new String[] {runtime,(String)iter2.next()});
		}
		initSelection();
		System.out.println("ic: "+availablePrologRuntimes.getItemCount());
		while (!dialogShell.isDisposed()) {
			if (!shell.getDisplay().readAndDispatch())
				shell.getDisplay().sleep();
		}
		return selected;


	}

	private void init() {
		dialogShell = new org.eclipse.swt.widgets.Shell(shell, SWT.DIALOG_TRIM);

		radioButtons = new Composite(dialogShell, SWT.NONE);
		GridLayout gridLayout = new GridLayout(2,false);
		radioButtons.setLayout(gridLayout);
		FormData layoutRadioButtons = new FormData();
		layoutRadioButtons.left = new FormAttachment(1,100, 8);
		radioButtons.setLayoutData(layoutRadioButtons);

		availablePrologRuntimes = new Table(dialogShell, SWT.BORDER | SWT.SINGLE);
		availablePrologRuntimes.setHeaderVisible(true);
		
		addReferencedProjects = new Button(dialogShell, SWT.CHECK);
		addReferencedProjects.setText("include all referenced projects");

		FormData referencedProjectsLayout = new FormData();
		referencedProjectsLayout.top = new FormAttachment(availablePrologRuntimes,10);
		referencedProjectsLayout.left = new FormAttachment(1, 100, 10);
//		referencedProjectsLayout.right = new FormAttachment(99, 100, 0);
		addReferencedProjects.setLayoutData(referencedProjectsLayout);
		
		availablePrologRuntimes.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			public void widgetSelected(SelectionEvent e) {
				updateSelection(availablePrologRuntimes.getSelection()[0].getText());
			}
			
		});
		
		//availablePrologRuntimes.setLinesVisible(true);
		final TableColumn column = new TableColumn(availablePrologRuntimes,SWT.NONE);
		column.setText("Factbase");
		column.setResizable(true);
		column.setWidth(100);
		final TableColumn column2 = new TableColumn(availablePrologRuntimes,SWT.NONE);
		column2.setText("Used / Shared by");
		column2.setResizable(true);
		column2.setWidth(337);
		Label line = new Label(dialogShell,SWT.HORIZONTAL|SWT.SEPARATOR);
		
		FormData formDataLine = new FormData();
		formDataLine.width = 580;

		formDataLine.top = new FormAttachment(addReferencedProjects,5);//new FormAttachment(label,3);
		line.setLayoutData(formDataLine);
		buttons = new Composite(dialogShell, SWT.NONE);
		buttons.setLayout(new FormLayout());
		
		FormData formData = new FormData();
		formData.top = new FormAttachment(radioButtons,1);
		formData.left = new FormAttachment(1,10);
		formData.width = 420;
		formData.height = 200;

//		Label label = new Label(dialogShell, SWT.WRAP);
//		label.setText("Do you want to have a dedicated factbase named '"+ projectRuntime + "' for this project or do you want to join the factbase of another project?");
		FormData formDataLabel = new FormData();
		formDataLabel.width = 330;
		formDataLabel.left = new FormAttachment(1,10);
		
		formDataLabel.top = new FormAttachment(availablePrologRuntimes,5);
		formDataLabel.height = 40;
//		label.setLayoutData(formDataLabel);
		
		FormData formDataComposite = new FormData();
		formDataComposite.width = 580;

		formDataComposite.top = new FormAttachment(line,5);//new FormAttachment(label,3);
		formDataComposite.height = 30;
		//formDataComposite.bottom = new FormAttachment(80,100,5);
		
		availablePrologRuntimes.setLayoutData(formData);
		availablePrologRuntimes.setEnabled(false);
		buttons.setLayoutData(formDataComposite);
		
		createButtons();

		dialogShell.setLayout(new FormLayout());
		dialogShell.setSize(new Point(480, 400));
		okButton.setFocus();
		dialogShell.addShellListener(new org.eclipse.swt.events.ShellAdapter() {
			public void shellClosed(org.eclipse.swt.events.ShellEvent e) {
				if (!isClosing) {
					
					e.doit = cancelled();
				}
			}
		});
	}


	private boolean doExit() {
		// in the default case
		if(selected.length() > 0) {
			if( radioDefault.getSelection()){
				lastRuntime = null;
			} else {
				lastRuntime = selected;
			}
			includeReferencedProjects = addReferencedProjects.getSelection();

		} 
		isClosing = true;
		dialogShell.close();
		dialogShell.dispose();
		return true;
	}
	
	public boolean isIncludeReferencedProjects() {
		return includeReferencedProjects; 
	}
	
	public class ToggleRadioButtonSelection implements SelectionListener {

		private Button selectedButton;
		private Composite radioButtons;

		public ToggleRadioButtonSelection(Composite radioButtons, Button selectedButton) {
			this.radioButtons = radioButtons;
			this.selectedButton = selectedButton; 
		}

		public void widgetDefaultSelected(SelectionEvent e) {
		}

		public void widgetSelected(SelectionEvent e) {
			if(selectedButton == radioSelect) {
				if(availablePrologRuntimes.getItemCount() > 0){
					availablePrologRuntimes.setEnabled(selectedButton == radioSelect);
					
					if(availablePrologRuntimes.getSelection().length > 0){
						updateSelection(availablePrologRuntimes.getSelection()[0].getText());
					} 
				}else {
					return;
				}
			}
			availablePrologRuntimes.setEnabled(selectedButton == radioSelect);
			name.setEnabled(selectedButton == radioNamed);


			for (int i = 0; i < radioButtons.getChildren().length; i++) {
				if(radioButtons.getChildren()[i] instanceof Button) {
					Button button = (Button)radioButtons.getChildren()[i];
					button.setSelection(button == selectedButton);
				}
			}
		}

	}

}

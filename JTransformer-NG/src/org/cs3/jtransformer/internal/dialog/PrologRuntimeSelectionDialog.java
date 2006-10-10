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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;

import sun.security.krb5.internal.s;

public class PrologRuntimeSelectionDialog {


	private Shell dialogShell = null;  //  @jve:decl-index=0:visual-constraint="67,7"

	private static String lastRuntime = null;

	private boolean isClosing = false;

	private String selected = null;

	private Table availablePrologRuntimes = null;

	private Composite buttons = null;

	private Button newButton = null;

	private Button joinButton = null;

	private Button cancelButton = null;

	private Shell shell;

	private List runtimes;

	private String projectRuntime;

	private List subscriptions;
	
	public PrologRuntimeSelectionDialog(Shell shell, List runtimes,List subscriptions, String projectRuntime) {
		this.shell = shell;
		this.runtimes = runtimes;
		this.subscriptions = subscriptions;
		this.projectRuntime = projectRuntime;
		init();
	}
	
	/**
	 * This method initializes buttons	
	 *
	 */
	
	
	private void createButtons() {

		newButton = new Button(buttons, SWT.NONE);
		newButton.setText("Dedicated Factbase");
		joinButton = new Button(buttons, SWT.NONE);
		joinButton.setText("Join");
		cancelButton = new Button(buttons, SWT.NONE);
		cancelButton.setText("Cancel");

		FormData formDataNew = new FormData();
		formDataNew.left = new FormAttachment(1, 40,1);

		FormData formDataJoin = new FormData();
		formDataJoin.left = new FormAttachment(4, 10,10);
		FormData formDataCancel = new FormData();
		formDataCancel.right = new FormAttachment(9, 10,10);
		newButton.setLayoutData(formDataNew);
		joinButton.setLayoutData(formDataJoin);
		cancelButton.setLayoutData(formDataCancel);

		
		newButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			public void widgetSelected(SelectionEvent e) {
				selected = projectRuntime;
				lastRuntime = selected;
				doExit();
			}
			
		});
		
		joinButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			public void widgetSelected(SelectionEvent e) {
				selected = availablePrologRuntimes.getSelection()[0].getText();
				doExit();
			}
			
		});
		
		cancelButton.addSelectionListener(new SelectionListener() {

			public void widgetDefaultSelected(SelectionEvent e) {
			}

			public void widgetSelected(SelectionEvent e) {
				selected = null;
				doExit();
			}
			
		});
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

//		while (!thisClass.dialogShell.isDisposed()) {
//			if (!display.readAndDispatch())
//				display.sleep();
//		}
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
		if(runtimes.size() > 0) {
			int indexOfLast = runtimes.indexOf(lastRuntime);
			if(indexOfLast > 0) {
				availablePrologRuntimes.select(indexOfLast);
			} else {
				availablePrologRuntimes.select(0);
			}
		}
		System.out.println("ic: "+availablePrologRuntimes.getItemCount());
		while (!dialogShell.isDisposed()) {
			if (!shell.getDisplay().readAndDispatch())
				shell.getDisplay().sleep();
		}
		return selected;


	}

	private void init() {
		dialogShell = new org.eclipse.swt.widgets.Shell(shell, SWT.DIALOG_TRIM);
		
		availablePrologRuntimes = new Table(dialogShell, SWT.BORDER | SWT.SINGLE);
		availablePrologRuntimes.setHeaderVisible(true);
		//availablePrologRuntimes.setLinesVisible(true);
		final TableColumn column = new TableColumn(availablePrologRuntimes,SWT.NONE);
		column.setText("Factbase");
		column.setResizable(true);
		column.setWidth(100);
		final TableColumn column2 = new TableColumn(availablePrologRuntimes,SWT.NONE);
		column2.setText("Used / Shared by");
		column2.setResizable(true);
		column2.setWidth(237);
		buttons = new Composite(dialogShell, SWT.NONE);
		buttons.setLayout(new FormLayout());
		
		FormData formData = new FormData();
		formData.top = new FormAttachment(1,1);
		formData.left = new FormAttachment(1,10);
		formData.width = 320;
		formData.height = 200;

		Label label = new Label(dialogShell, SWT.WRAP);
		label.setText("Do you want to have a dedicated factbase named '"+ projectRuntime + "' for this project or do you want to join the factbase of another project?");

		FormData formDataLabel = new FormData();
		formDataLabel.width = 330;
		formDataLabel.left = new FormAttachment(1,10);
		
		formDataLabel.top = new FormAttachment(availablePrologRuntimes,5);
		formDataLabel.height = 40;
		label.setLayoutData(formDataLabel);

		
		FormData formDataComposite = new FormData();
		formDataComposite.width = 380;
		formDataComposite.top = new FormAttachment(label,3);
		formDataComposite.height = 30;
		//formDataComposite.bottom = new FormAttachment(80,100,5);
		
		availablePrologRuntimes.setLayoutData(formData);
		buttons.setLayoutData(formDataComposite);
//		availablePrologRuntimes.addSelectionListener(new SelectionListener() {
//
//			public void widgetDefaultSelected(SelectionEvent e) {
//				// TODO Auto-generated method stub
//				
//			}
//
//			public void widgetSelected(SelectionEvent e) {
//				selected = ((TableItem)e.item).getText(); 
//				
//			}
//			
//		});
		
		createButtons();

		dialogShell.setText("Add JTransformer Nature");
		dialogShell.setLayout(new FormLayout());
		dialogShell.setSize(new Point(380, 340));
		newButton.setFocus();
		dialogShell.addShellListener(new org.eclipse.swt.events.ShellAdapter() {
			public void shellClosed(org.eclipse.swt.events.ShellEvent e) {
				if (!isClosing) {
					e.doit = doExit();
				}
			}
		});
	}



	private boolean doExit() {
		isClosing = true;
		dialogShell.close();
		dialogShell.dispose();
		//shell.close();
		return true;
	}
}

/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.internal.preferences;

import java.io.IOException;
import java.util.List;
import java.util.Set;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntime;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.ui.PrologRuntimeUI;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.ui.util.preferences.MyBooleanFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyDirectoryFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyFileFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyIntegerFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyLabelFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyStringFieldEditor;
import org.cs3.prolog.ui.util.preferences.StructuredFieldEditorPreferencePage;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

/**
 * This class represents a preference page that is contributed to the
 * Preferences dialog. By subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows us to create a page
 * that is small and knows how to save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They are stored in the
 * preference store that belongs to the main plug-in class. That way,
 * preferences can be accessed directly via the preference store.
 */

public class PreferencePage extends StructuredFieldEditorPreferencePage implements IWorkbenchPreferencePage {
	
	
	private boolean isNewPrefExecutable = false;
	
	private MyFileFieldEditor executable;
	private MyStringFieldEditor invocation;
	private MyStringFieldEditor commandLineArguments;
	private MyStringFieldEditor startupFiles;
	private MyLabelFieldEditor executeablePreviewLabel;
	private MyBooleanFieldEditor metaPred;
	private MyStringFieldEditor extraEnvironmentVariables;
	private MyDirectoryFieldEditor serverLogDir;
	private MyIntegerFieldEditor timeoutFieldEditor;
	private MyBooleanFieldEditor hidePrologWindow;
	private MyBooleanFieldEditor genFactbase;

	private Composite configurationSelector;

	private Combo configurationList;

	private Button newConfig;

	private Button deleteConfig;

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(PrologRuntimeUIPlugin.getDefault().getPreferenceStore());
		setDescription("Select a predefined configuration or define a new one. Each configuration affects all settings on this page.");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		configurationSelector = createConfigurationSelector(getFieldEditorParent());
		
		Group executableGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		executableGroup.setText("Executable");
		
		invocation = new MyStringFieldEditor(PrologRuntime.PREF_INVOCATION, "OS invocation", executableGroup);
		addField(invocation);
		
		// eg. xpce or /usr/bin/xpce
		executable = new MyFileFieldEditor(PrologRuntime.PREF_EXECUTABLE, "Prolog executable", executableGroup);
		executable.getLabelControl(executableGroup).setToolTipText("Don't enter quotes, they will be added automatically.");
		addField(executable);
		
		commandLineArguments = new MyStringFieldEditor(PrologRuntime.PREF_COMMAND_LINE_ARGUMENTS, "Command line arguments", executableGroup);
		commandLineArguments.getLabelControl(executableGroup).setToolTipText("See SWI-Prolog manual for a list of possible command line arguments.");
		addField(commandLineArguments);
		
		startupFiles = new MyStringFieldEditor(PrologRuntime.PREF_ADDITIONAL_STARTUP, "Additional startup files", executableGroup) {
			@Override
			protected boolean doCheckState() {
				String value = getStringValue();
				String[] files = value.split(",");
				for (String file : files) {
					if (file.contains(" ")) {
						if (!(file.startsWith("\"") && file.endsWith("\""))
								&& !(file.startsWith("'") && file.endsWith("'"))) {
							return false;
						}
					}
				}
				return true;
			}
		};
		startupFiles.setErrorMessage("File paths containing white spaces must be enclosed in double quotes. To enter multiple files, separate them by a comma.");
		startupFiles.getLabelControl(executableGroup).setToolTipText("Can be multiple files, seperated by commas.\nAdd quotes if needed!\n\nExample: \"c:/my files/dummy.pl\" dummy2.pl");

		addField(startupFiles);

		executeablePreviewLabel = new MyLabelFieldEditor(executableGroup, "Executable preview");
		addField(executeablePreviewLabel);
		
		extraEnvironmentVariables = new MyStringFieldEditor(PrologRuntime.PREF_ENVIRONMENT, "Extra environment variables", getFieldEditorParent());
		addField(extraEnvironmentVariables);
		
		serverLogDir = new MyDirectoryFieldEditor(PrologRuntime.PREF_SERVER_LOGDIR, "Server-Log file location", getFieldEditorParent());
		addField(serverLogDir);
		
		timeoutFieldEditor = new MyIntegerFieldEditor(PrologRuntime.PREF_TIMEOUT, "Connect Timeout", getFieldEditorParent());
		timeoutFieldEditor.getTextControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		timeoutFieldEditor.getLabelControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		addField(timeoutFieldEditor);

		// The host the PIF server is listening on
		StringFieldEditor host = new MyStringFieldEditor(PrologRuntime.PREF_HOST, "Server host", getFieldEditorParent());
		host.setEnabled(false, getFieldEditorParent());
		addField(host);

		// The port the PIF server is listening on
		IntegerFieldEditor port = new MyIntegerFieldEditor(PrologRuntime.PREF_PORT, "Server port", getFieldEditorParent());
		port.setEnabled(false, getFieldEditorParent());
		addField(port);
		
		hidePrologWindow = new MyBooleanFieldEditor(PrologRuntime.PREF_HIDE_PLWIN, "Hide prolog process window (Windows only)", getFieldEditorParent());
		addField(hidePrologWindow);
		
		genFactbase = new MyBooleanFieldEditor(PrologRuntime.PREF_GENERATE_FACTBASE, "Experimental: Create prolog metadata", getFieldEditorParent()){
			@Override
			public void doLoad(){
				super.doLoad();
				getMetaPredEditor().setEnabled(getBooleanValue(), getFieldEditorParent());
			}
			
			@Override
			public void doLoadDefault(){
				super.doLoadDefault();
				getMetaPredEditor().setEnabled(getBooleanValue(), getFieldEditorParent());
			}
		};
		genFactbase.getDescriptionControl(getFieldEditorParent()).setToolTipText("This may take a while on large files");
		metaPred = new MyBooleanFieldEditor(PrologRuntime.PREF_META_PRED_ANALYSIS, "Experimental: Run meta predicate analysis after loading a prolog file", getFieldEditorParent());
		genFactbase.getDescriptionControl(getFieldEditorParent()).addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				metaPred.setEnabled(genFactbase.getBooleanValue(), getFieldEditorParent());
			}
		});
		addField(genFactbase);
		addField(metaPred);
		
		adjustLayoutForElement(configurationSelector);
		adjustLayoutForElement(executableGroup);
	}

	@Override
	protected void initialize() {
		super.initialize();
		updateExecuteablePreviewLabelText();
		fillConfigLabels();
		selectConfig(getPreferenceStore().getString(PrologRuntimeUI.PREF_CONFIGURATION));
	}

	private BooleanFieldEditor getMetaPredEditor(){
		return metaPred;
	}
	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	@Override
	public void init(IWorkbench workbench) {
	}
	
	@Override
	public void propertyChange(PropertyChangeEvent event) {
		super.propertyChange(event);
		
		String prefName = ((FieldEditor)event.getSource()).getPreferenceName();
		if(prefName.equals(PrologRuntime.PREF_INVOCATION) 
				|| prefName.equals(PrologRuntime.PREF_EXECUTABLE)
				|| prefName.equals(PrologRuntime.PREF_ADDITIONAL_STARTUP)
				|| prefName.equals(PrologRuntime.PREF_COMMAND_LINE_ARGUMENTS)) {
			
			isNewPrefExecutable = true;
			updateExecuteablePreviewLabelText();
		} else if (prefName.equals(PrologRuntimeUI.PREF_CONFIGURATION)) {
			setValues(PreferenceConfiguration.getInstance().getPreferenceStore(event.getNewValue().toString()));
		}
    }
	
	@Override
	public boolean performOk() {
		getPreferenceStore().setValue(PrologRuntimeUI.PREF_CONFIGURATION, getIdForLabel(configurationList.getText()));
		saveValuesToSpecificStore();
		if(isNewPrefExecutable) {
			updatePrologInterfaceExecutables();	
		}
		return super.performOk();
	}
	
	private void updateExecuteablePreviewLabelText() {
		String newExecutable = Util.createExecutable(invocation.getStringValue(), executable.getStringValue(), commandLineArguments.getStringValue(), startupFiles.getStringValue());
		executeablePreviewLabel.setText(newExecutable);
	}

	private void updatePrologInterfaceExecutables() {

		String newExecutable = Util.createExecutable(invocation.getStringValue(), executable.getStringValue(), commandLineArguments.getStringValue(), startupFiles.getStringValue());
		
		PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		Set<String> subscriptionIds = registry.getAllSubscriptionIDs();
		for (String id : subscriptionIds) {
			PrologInterface pif = registry.getPrologInterface(registry.getSubscription(id).getPifKey());
			if(pif != null && !(pif.isDown()) ){   // Sinan & Günter, 24.9.2010
				pif.setExecutable(newExecutable);
			}
		}
	}
	
	@Override
	protected void performApply() {
		performOk();
		isNewPrefExecutable=false;
	}
	
    @Override
	protected void performDefaults() {
    	setDefaultValues(PreferenceConfiguration.getInstance().getPreferenceStore(getIdForLabel(configurationList.getText())));
        checkState();
        updateApplyButton();
    }

	
	private void setValues(PreferenceStore store) {
		if (store != null) {
			executable.setStringValue(store.getString(executable.getPreferenceName()));
			invocation.setStringValue(store.getString(invocation.getPreferenceName()));
			commandLineArguments.setStringValue(store.getString(commandLineArguments.getPreferenceName()));
			startupFiles.setStringValue(store.getString(startupFiles.getPreferenceName()));
			metaPred.setBooleanValue(store.getBoolean(metaPred.getPreferenceName()));
			extraEnvironmentVariables.setStringValue(store.getString(extraEnvironmentVariables.getPreferenceName()));
			serverLogDir.setStringValue(store.getString(serverLogDir.getPreferenceName()));
			timeoutFieldEditor.setIntValue(store.getInt(timeoutFieldEditor.getPreferenceName()));
			hidePrologWindow.setBooleanValue(store.getBoolean(hidePrologWindow.getPreferenceName()));
			genFactbase.setBooleanValue(store.getBoolean(genFactbase.getPreferenceName()));
		}
	}
	
	private void setDefaultValues(PreferenceStore store) {
		if (store != null) {
			executable.setStringValue(store.getDefaultString(executable.getPreferenceName()));
			invocation.setStringValue(store.getDefaultString(invocation.getPreferenceName()));
			commandLineArguments.setStringValue(store.getDefaultString(commandLineArguments.getPreferenceName()));
			startupFiles.setStringValue(store.getDefaultString(startupFiles.getPreferenceName()));
			metaPred.setBooleanValue(store.getDefaultBoolean(metaPred.getPreferenceName()));
			extraEnvironmentVariables.setStringValue(store.getDefaultString(extraEnvironmentVariables.getPreferenceName()));
			serverLogDir.setStringValue(store.getDefaultString(serverLogDir.getPreferenceName()));
			timeoutFieldEditor.setIntValue(store.getDefaultInt(timeoutFieldEditor.getPreferenceName()));
			hidePrologWindow.setBooleanValue(store.getDefaultBoolean(hidePrologWindow.getPreferenceName()));
			genFactbase.setBooleanValue(store.getDefaultBoolean(genFactbase.getPreferenceName()));
		}
	}
	
	private void saveValuesToSpecificStore() {
		PreferenceStore store = PreferenceConfiguration.getInstance().getPreferenceStore(getIdForLabel(configurationList.getText()));
		if (store != null) {
			store.setValue(executable.getPreferenceName(), executable.getStringValue());
			store.setValue(invocation.getPreferenceName(), invocation.getStringValue());
			store.setValue(commandLineArguments.getPreferenceName(), commandLineArguments.getStringValue());
			store.setValue(startupFiles.getPreferenceName(), startupFiles.getStringValue());
			store.setValue(metaPred.getPreferenceName(), metaPred.getBooleanValue());
			store.setValue(extraEnvironmentVariables.getPreferenceName(), extraEnvironmentVariables.getStringValue());
			store.setValue(serverLogDir.getPreferenceName(), serverLogDir.getStringValue());
			store.setValue(timeoutFieldEditor.getPreferenceName(), timeoutFieldEditor.getIntValue());
			store.setValue(hidePrologWindow.getPreferenceName(), hidePrologWindow.getBooleanValue());
			store.setValue(genFactbase.getPreferenceName(), genFactbase.getBooleanValue());
			try {
				store.save();
			} catch (IOException e) {
				Debug.report(e);
			}
		}
	}
	
	private Composite createConfigurationSelector(Composite parent) {
		Composite container = new Composite(parent, SWT.NONE);
		container.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		container.setLayout(new GridLayout(4, false));
		
		Label label = new Label(container, SWT.LEFT);
		label.setText("Configuration");
		label.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
		
		configurationList = new Combo(container, SWT.READ_ONLY);
		configurationList.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		configurationList.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent evt) {
				String configurationId = getIdForLabel(configurationList.getText());
				setValues(PreferenceConfiguration.getInstance().getPreferenceStore(configurationId));
				deleteConfig.setEnabled(!PreferenceConfiguration.getInstance().getDefaultConfigurationIds().contains(configurationId));
			}
		});
		
		newConfig = createButton(container, "New...");
		newConfig.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				NewConfigurationDialog dialog = new NewConfigurationDialog(newConfig.getShell(), getIdAndLabelArray(PreferenceConfiguration.getInstance().getDefaultConfigurationIds()));
				int result = dialog.open();
				if (result == Dialog.OK && dialog.getText() != null && !dialog.getText().isEmpty()) {
					String newConfigurationId = PreferenceConfiguration.getInstance().newConfigurationId(dialog.getId(), dialog.getText());
					fillConfigLabels();
					selectConfig(newConfigurationId);
					setValues(PreferenceConfiguration.getInstance().getPreferenceStore(newConfigurationId));
				}
			}
		});
		deleteConfig = createButton(container, "Delete");
		deleteConfig.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				boolean answer = MessageDialog.openQuestion(deleteConfig.getShell(), "Delete configuration", "Do you want to delete the configuration \"" + configurationList.getText() + "\"?");
				if (answer) {
					String configurationId = getIdForLabel(configurationList.getText());
					String defaultId = PreferenceConfiguration.getInstance().getDefaultId(configurationId);
					PreferenceConfiguration.getInstance().deleteConfiguration(configurationId);
					fillConfigLabels();
					selectConfig(defaultId);
					setValues(PreferenceConfiguration.getInstance().getPreferenceStore(defaultId));
				}
			}
		});
//		Label label5 = new Label(container, SWT.NONE);
//		label5.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 4, 1));
//		label5.setText(" ");
		return container;
	}
	
	private Button createButton(Composite parent, String label) {
		Button button = new Button(parent, SWT.PUSH);
		button.setText(label);
		
		GridData data = new GridData(SWT.FILL, SWT.CENTER, false, false);
		int widthHint = convertHorizontalDLUsToPixels(IDialogConstants.BUTTON_WIDTH);
		Point minSize = button.computeSize(SWT.DEFAULT, SWT.DEFAULT, true);
		data.widthHint = Math.max(widthHint, minSize.x);
		button.setLayoutData(data);
		return button;
	}
	
	private String[][] configLabels;
	
	private void fillConfigLabels() {
		configLabels = getIdAndLabelArray(PreferenceConfiguration.getInstance().getConfigurationIds());
		configurationList.removeAll();
		for (String[] idAndLabel : configLabels) {
			configurationList.add(idAndLabel[1]);
		}
	}
	
	private String[][] getIdAndLabelArray(List<String> ids) {
		String[][] idsAndLabels = new String[ids.size()][0];
		int i = 0;
		for (String configurationId : ids) {
			idsAndLabels[i] = new String[]{configurationId, PreferenceConfiguration.getInstance().getLabel(configurationId)};
			i++;
		}
		return idsAndLabels;
	}
	
	private String getIdForLabel(String label) {
		for (String[] idAndLabel : configLabels) {
			if (idAndLabel[1].equals(label)) {
				return idAndLabel[0];
			}
		}
		return null;
	}

	private void selectConfig(String configurationId) {
		configurationList.setText(PreferenceConfiguration.getInstance().getLabel(configurationId));
		deleteConfig.setEnabled(!PreferenceConfiguration.getInstance().getDefaultConfigurationIds().contains(configurationId));
	}
	
	private static class NewConfigurationDialog extends Dialog {

		private Text text;
		private String[][] configLabels;
		private Combo combo;
		
		private String id;
		private String label;

		protected NewConfigurationDialog(Shell parentShell, String[][] configLabels) {
			super(parentShell);
			this.configLabels = configLabels;
		}
		
		@Override
		protected Control createDialogArea(Composite parent) {
			Composite composite = (Composite) super.createDialogArea(parent);
			composite.setLayout(new GridLayout(2, false));
			
			Label label1 = new Label(composite, SWT.NONE);
			label1.setText("Name");
			label1.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
			
			text = new Text(composite, SWT.SINGLE | SWT.BORDER);
			text.setTextLimit(50);
			GridData textLayoutData = new GridData(SWT.FILL, SWT.CENTER, true, false);
			textLayoutData.widthHint = convertWidthInCharsToPixels(50);
			text.setLayoutData(textLayoutData);
			
			Label label2 = new Label(composite, SWT.NONE);
			label2.setText("Inherit defaults from");
			label2.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
			
			combo = new Combo(composite, SWT.READ_ONLY);
			for (String[] idAndLabel : configLabels) {
				combo.add(idAndLabel[1]);
			}
			combo.setText(configLabels[0][1]);
			combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			
			return composite;
		}
		
		@Override
		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText("New Configuration");
		}
		
		protected void okPressed() {
			label = text.getText();
			for (int i = 0; i < configLabels.length; i++) {
				String selectedLabel = combo.getText();
				if (selectedLabel.equals(configLabels[i][1])) {
					id = configLabels[i][0];
				}
			}
			if (id == null) {
				id = configLabels[0][0];
			}
			super.okPressed();
		}
		
		public String getText() {
			return label;
		}
		
		public String getId() {
			return id;
		}

	}
	
}



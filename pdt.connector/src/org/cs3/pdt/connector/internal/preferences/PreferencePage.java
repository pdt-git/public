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

package org.cs3.pdt.connector.internal.preferences;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.connector.PrologRuntimeUI;
import org.cs3.pdt.connector.PrologRuntimeUIPlugin;
import org.cs3.pdt.connector.registry.PrologInterfaceRegistry;
import org.cs3.pdt.connector.util.EclipsePreferenceProvider;
import org.cs3.pdt.connector.util.preferences.MyBooleanFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyDirectoryFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyFileFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyIntegerFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyLabelFieldEditor;
import org.cs3.pdt.connector.util.preferences.MyStringFieldEditor;
import org.cs3.pdt.connector.util.preferences.StructuredFieldEditorPreferencePage;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.pif.PrologInterface;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
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
	
	
	private boolean preferencesChanged = false;
	
	private MyFileFieldEditor executable;
	private MyStringFieldEditor invocation;
	private MyStringFieldEditor commandLineArguments;
	private MyStringFieldEditor startupFiles;
	private MyLabelFieldEditor executeablePreviewLabel;
	private MyStringFieldEditor extraEnvironmentVariables;
	private MyDirectoryFieldEditor serverLogDir;
	private MyIntegerFieldEditor timeoutFieldEditor;
	private MyBooleanFieldEditor hidePrologWindow;
	
	private ArrayList<FieldEditor> editors = new ArrayList<FieldEditor>();

	private Composite configurationSelector;

	private Combo configurationList;

	private Button newConfiguration;

	private Button deleteConfiguration;

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(PreferenceConfiguration.getInstance().getPreferenceStore(PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(PrologRuntimeUI.PREF_CONFIGURATION)));
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
		
		invocation = new MyStringFieldEditor(Connector.PREF_INVOCATION, "OS invocation", executableGroup);
		addField(invocation);
		
		// eg. xpce or /usr/bin/xpce
		executable = new MyFileFieldEditor(Connector.PREF_EXECUTABLE, "Prolog executable", executableGroup);
		executable.getLabelControl(executableGroup).setToolTipText("Don't enter quotes, they will be added automatically.");
		addField(executable);
		
		commandLineArguments = new MyStringFieldEditor(Connector.PREF_COMMAND_LINE_ARGUMENTS, "Command line arguments", executableGroup);
		commandLineArguments.getLabelControl(executableGroup).setToolTipText("See SWI-Prolog manual for a list of possible command line arguments.");
		addField(commandLineArguments);
		
		startupFiles = new MyStringFieldEditor(Connector.PREF_ADDITIONAL_STARTUP, "Additional startup files", executableGroup) {
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
		
		extraEnvironmentVariables = new MyStringFieldEditor(Connector.PREF_ENVIRONMENT, "Extra environment variables", getFieldEditorParent());
		addField(extraEnvironmentVariables);
		
		serverLogDir = new MyDirectoryFieldEditor(Connector.PREF_SERVER_LOGDIR, "Server-Log file location", getFieldEditorParent());
		addField(serverLogDir);
		
		timeoutFieldEditor = new MyIntegerFieldEditor(Connector.PREF_TIMEOUT, "Connect Timeout", getFieldEditorParent());
		timeoutFieldEditor.getTextControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		timeoutFieldEditor.getLabelControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		addField(timeoutFieldEditor);

		// The host the PIF server is listening on
		StringFieldEditor host = new MyStringFieldEditor(Connector.PREF_HOST, "Server host", getFieldEditorParent());
		host.setEnabled(false, getFieldEditorParent());
		addField(host);

		// The port the PIF server is listening on
		IntegerFieldEditor port = new MyIntegerFieldEditor(Connector.PREF_PORT, "Server port", getFieldEditorParent());
		port.setEnabled(false, getFieldEditorParent());
		addField(port);
		
		hidePrologWindow = new MyBooleanFieldEditor(Connector.PREF_HIDE_PLWIN, "Hide prolog process window (Windows only)", getFieldEditorParent());
		addField(hidePrologWindow);
		
		adjustLayoutForElement(configurationSelector);
		adjustLayoutForElement(executableGroup);
	}

	@Override
	protected void initialize() {
		super.initialize();
		updateExecuteablePreviewLabelText();
		fillConfigurationList();
		selectConfiguration(PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(PrologRuntimeUI.PREF_CONFIGURATION));
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
		if (prefName.equals(Connector.PREF_INVOCATION) 
				|| prefName.equals(Connector.PREF_EXECUTABLE)
				|| prefName.equals(Connector.PREF_ADDITIONAL_STARTUP)
				|| prefName.equals(Connector.PREF_COMMAND_LINE_ARGUMENTS)) {
			
			updateExecuteablePreviewLabelText();
		}
		preferencesChanged = true;
    }
	
	@Override
	public void addField(FieldEditor editor) {
		editors.add(editor);
		super.addField(editor);
	}
	
	private void changePreferenceStore(PreferenceStore store) {
		setPreferenceStore(store);
		for (FieldEditor editor : editors) {
			editor.setPreferenceStore(store);
			editor.load();
		}
		updateExecuteablePreviewLabelText();
	}
	
	@Override
	public boolean performOk() {
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(PrologRuntimeUI.PREF_CONFIGURATION, configurationList.getText());
		boolean result = super.performOk();
		try {
			((PreferenceStore)getPreferenceStore()).save();
		} catch (Exception e) {
			Debug.report(e);
		}
		if (preferencesChanged) {
			updatePrologInterfaceExecutables();	
		}
		return result;
	}
	
	private void updateExecuteablePreviewLabelText() {
		String newExecutable = Util.createExecutable(invocation.getStringValue(), executable.getStringValue(), commandLineArguments.getStringValue(), startupFiles.getStringValue()) + " -g [$ConnectorInitFile]";
		executeablePreviewLabel.setText(newExecutable);
	}

	private void updatePrologInterfaceExecutables() {
		String configuration = configurationList.getText();
		PrologInterfaceRegistry registry = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceRegistry();
		Set<String> subscriptionIds = registry.getAllSubscriptionIDs();
		for (String id : subscriptionIds) {
			PrologInterface pif = registry.getPrologInterface(registry.getSubscription(id).getPifKey());
			if (pif != null && configuration.equals(pif.getAttribute(PrologRuntimeUI.CONFIGURATION_ATTRIBUTE))) {   // Sinan & Günter, 24.9.2010
				pif.initOptions(new EclipsePreferenceProvider(PrologRuntimeUIPlugin.getDefault(), configuration));
			}
		}
	}
	
	@Override
	protected void performApply() {
		performOk();
		preferencesChanged = false;
	}
	
    @Override
	protected void performDefaults() {
    	super.performDefaults();
//        checkState();
//        updateApplyButton();
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
				String configuration = configurationList.getText();
				changePreferenceStore(PreferenceConfiguration.getInstance().getPreferenceStore(configuration));
				deleteConfiguration.setEnabled(!PreferenceConfiguration.getInstance().getDefaultConfigurations().contains(configuration));
			}
		});
		
		newConfiguration = createButton(container, "New...");
		newConfiguration.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				NewConfigurationDialog dialog = new NewConfigurationDialog(newConfiguration.getShell(), PreferenceConfiguration.getInstance().getConfigurations(), PreferenceConfiguration.getInstance().getDefaultConfigurations());
				int result = dialog.open();
				if (result == Dialog.OK && dialog.getConfiguration() != null && !dialog.getConfiguration().isEmpty()) {
					String newConfiguration = dialog.getConfiguration();
					PreferenceConfiguration.getInstance().addConfiguration(newConfiguration, dialog.getDefaultConfiguration());
					fillConfigurationList();
					selectConfiguration(newConfiguration);
					changePreferenceStore(PreferenceConfiguration.getInstance().getPreferenceStore(newConfiguration));
				}
			}
		});
		deleteConfiguration = createButton(container, "Delete");
		deleteConfiguration.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				boolean answer = MessageDialog.openQuestion(deleteConfiguration.getShell(), "Delete configuration", "Do you want to delete the configuration \"" + configurationList.getText() + "\"?");
				if (answer) {
					String configuration = configurationList.getText();
					String defaultId = PreferenceConfiguration.getInstance().getDefaultConfiguration(configuration);
					PreferenceConfiguration.getInstance().deleteConfiguration(configuration);
					fillConfigurationList();
					selectConfiguration(defaultId);
					changePreferenceStore(PreferenceConfiguration.getInstance().getPreferenceStore(defaultId));
					PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(PrologRuntimeUI.PREF_CONFIGURATION, defaultId);
				}
			}
		});
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
	
	private void fillConfigurationList() {
		configurationList.removeAll();
		for (String configId : PreferenceConfiguration.getInstance().getConfigurations()) {
			configurationList.add(configId);
		}
	}
	
	private void selectConfiguration(String configurationId) {
		configurationList.setText(configurationId);
		deleteConfiguration.setEnabled(!PreferenceConfiguration.getInstance().getDefaultConfigurations().contains(configurationId));
	}
	
	private static class NewConfigurationDialog extends Dialog {

		private Text text;
		private List<String> configurations;
		private List<String> defaultConfigurations;
		private Combo combo;
		
		private String defaultConfiguration;
		private String configuration;

		private static final char[] ILLEGAL_CHARACTERS = { '/', '\n', '\r', '\t', '\0', '\f', '`', '?', '*', '\\', '<', '>', '|', '\"', ':', '\'', ';' };
		
		protected NewConfigurationDialog(Shell parentShell, List<String> configurations, List<String> defaultConfigurations) {
			super(parentShell);
			this.configurations = configurations;
			this.defaultConfigurations = defaultConfigurations;
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
			text.addVerifyListener(new VerifyListener() {
				@Override
				public void verifyText(VerifyEvent e) {
					for (char c : e.text.toCharArray()) {
						if (isIllegalCharacter(c)) {
							e.doit = false;
							return;
						}
					}
				}
				
				private boolean isIllegalCharacter(char c) {
					for (char illegal : ILLEGAL_CHARACTERS) {
						if (illegal == c) {
							return true;
						}
					}
					return false;
				}
			});
			
			Label label2 = new Label(composite, SWT.NONE);
			label2.setText("Inherit defaults from");
			label2.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false));
			
			combo = new Combo(composite, SWT.READ_ONLY);
			for (String defaultConfiguration : defaultConfigurations) {
				combo.add(defaultConfiguration);
			}
			combo.setText(defaultConfigurations.get(0));
			combo.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			
			return composite;
		}
		
		@Override
		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText("New Configuration");
		}
		
		protected void okPressed() {
			if (text.getText().isEmpty()) {
				MessageDialog.openWarning(getShell(), "New Configuration", "Configuration must not be empty.");
				return;
			} else if (configurations.contains(text.getText())) {
				MessageDialog.openWarning(getShell(), "New Configuration", "Configuration malready exists.");
				return;
			}
			configuration = text.getText();
			defaultConfiguration = combo.getText();
			super.okPressed();
		}
		
		public String getConfiguration() {
			return configuration;
		}
		
		public String getDefaultConfiguration() {
			return defaultConfiguration;
		}

	}
	
}



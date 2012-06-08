package org.cs3.prolog.connector.internal.preferences;

import java.util.Set;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntime;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.ui.util.preferences.MyBooleanFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyDirectoryFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyIntegerFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyLabelFieldEditor;
import org.cs3.prolog.ui.util.preferences.MyStringFieldEditor;
import org.cs3.prolog.ui.util.preferences.StructuredFieldEditorPreferencePage;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Listener;
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
	
	private StringFieldEditor executable;
	private StringFieldEditor invocation;
	private StringFieldEditor commandLineArguments;
	private StringFieldEditor startupFiles;
	private MyLabelFieldEditor executeablePreviewLabel;

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(PrologRuntimeUIPlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		Group executableGroup = new Group(getFieldEditorParent(), SWT.SHADOW_ETCHED_OUT);
		executableGroup.setText("Executable");
		
		invocation = new MyStringFieldEditor(PrologRuntime.PREF_INVOCATION, "Prolog invocation", executableGroup);
		addField(invocation);
		
		// eg. xpce or /usr/bin/xpce
		executable = new MyStringFieldEditor(PrologRuntime.PREF_EXECUTABLE, "Prolog executable", executableGroup);
		addField(executable);
		
		commandLineArguments = new MyStringFieldEditor(PrologRuntime.PREF_COMMAND_LINE_ARGUMENTS, "Command line arguments", executableGroup);
		addField(commandLineArguments);
		
		startupFiles = new MyStringFieldEditor(PrologRuntime.PREF_ADDITIONAL_STARTUP, "Additional startup files", executableGroup);
		addField(startupFiles);
		
		executeablePreviewLabel = new MyLabelFieldEditor(executableGroup, "Executable preview");
		addField(executeablePreviewLabel);
		
		// A comma-separated list of VARIABLE=VALUE pairs.
		addField(new MyStringFieldEditor(PrologRuntime.PREF_ENVIRONMENT, "Extra environment variables", getFieldEditorParent()));
		
		MyDirectoryFieldEditor ffe = new MyDirectoryFieldEditor(PrologRuntime.PREF_SERVER_LOGDIR, "Server-Log file location", getFieldEditorParent());
		addField(ffe);
		
		// Maximum time in milliseconds to wait for the prolog process to come up.
		IntegerFieldEditor timeoutFieldEditor = new MyIntegerFieldEditor(PrologRuntime.PREF_TIMEOUT, "Connect Timeout", getFieldEditorParent());
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
		
		addField(new MyBooleanFieldEditor(PrologRuntime.PREF_HIDE_PLWIN, "Hide prolog process window (Windows only)", getFieldEditorParent()));
		
		final MyBooleanFieldEditor genFactbase = new MyBooleanFieldEditor(PrologRuntime.PREF_GENERATE_FACTBASE, "Experimental: Create prolog metadata", getFieldEditorParent()){
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
		
		adjustLayoutForElement(executableGroup);
	}
	
	@Override
	protected void initialize() {
		super.initialize();
		updateExecuteablePreviewLabelText();
	}
	
	private BooleanFieldEditor metaPred;

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
		}
    }
	
	@Override
	public boolean performOk() {
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
		if(isNewPrefExecutable) {
			updatePrologInterfaceExecutables();	
		}
		isNewPrefExecutable=false;
	}
	
}
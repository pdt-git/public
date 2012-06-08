package org.cs3.pdt.runtime.preferences;

import java.util.Set;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.ui.preferences.MyLabelFieldEditor;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Event;
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

public class PreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {
	
	
	private boolean isNewPrefExecutable = false;
	
	private StringFieldEditor executable;
	private StringFieldEditor invocation;
	private StringFieldEditor commandLineArguments;
	private StringFieldEditor startupFiles;
	private MyLabelFieldEditor executeablePreviewLabel;

	public PreferencePage() {
		super(GRID);
		
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		setPreferenceStore(plugin.getPreferenceStore());

	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
		
		invocation = new StringFieldEditor(PrologRuntime.PREF_INVOCATION, "Prolog invocation", getFieldEditorParent());
		addField(invocation);
		
		// eg. xpce or /usr/bin/xpce
		executable = new StringFieldEditor(PrologRuntime.PREF_EXECUTABLE, "Prolog executable", getFieldEditorParent());
		addField(executable);

		commandLineArguments = new StringFieldEditor(PrologRuntime.PREF_COMMAND_LINE_ARGUMENTS, "Command line arguments", getFieldEditorParent());
		addField(commandLineArguments);
		
		startupFiles = new StringFieldEditor(PrologRuntime.PREF_ADDITIONAL_STARTUP, "Additional startup files", getFieldEditorParent());
		addField(startupFiles);
		
		executeablePreviewLabel = new MyLabelFieldEditor(getFieldEditorParent(), "Executable preview");
		addField(executeablePreviewLabel);
		
		// A comma-separated list of VARIABLE=VALUE pairs.
		addField(new StringFieldEditor(PrologRuntime.PREF_ENVIRONMENT, "Extra environment variables", getFieldEditorParent()));
		
		// The PrologInterface needs to temporarily store some
		// prolog files during bootstrapping. Any directory for which 
		// you have write permissions will do.
		addField(new DirectoryFieldEditor(PrologRuntime.PREF_PIF_BOOTSTRAP_DIR, "Prolog Process Bootstrap Directory", getFieldEditorParent()));

		DirectoryFieldEditor ffe = new DirectoryFieldEditor(PrologRuntime.PREF_SERVER_LOGDIR, "Server-Log file location", getFieldEditorParent());
		addField(ffe);
		
		// Maximum time in milliseconds to wait for the prolog process to come up.
		IntegerFieldEditor timeoutFieldEditor = new IntegerFieldEditor(PrologRuntime.PREF_TIMEOUT, "Connect Timeout", getFieldEditorParent());
		timeoutFieldEditor.getTextControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		timeoutFieldEditor.getLabelControl(getFieldEditorParent()).setToolTipText("Milliseconds to wait until connection to a new Prolog Process is established");
		addField(timeoutFieldEditor);

		// The host the PIF server is listening on
		StringFieldEditor host = new StringFieldEditor(PrologRuntime.PREF_HOST, "Server host", getFieldEditorParent());
		host.setEnabled(false, getFieldEditorParent());
		addField(host);

		// The port the PIF server is listening on
		IntegerFieldEditor port = new IntegerFieldEditor(PrologRuntime.PREF_PORT, "Server port", getFieldEditorParent());
		port.setEnabled(false, getFieldEditorParent());
		addField(port);
		
		addField(new BooleanFieldEditor(PrologRuntime.PREF_HIDE_PLWIN, "Hide prolog process window (Windows only)", getFieldEditorParent()));
		
		final BooleanFieldEditor genFactbase = new BooleanFieldEditor(PrologRuntime.PREF_GENERATE_FACTBASE, "Experimental: Create prolog metadata", getFieldEditorParent()){
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
		metaPred = new BooleanFieldEditor(PrologRuntime.PREF_META_PRED_ANALYSIS, "Experimental: Run meta predicate analysis after loading a prolog file", getFieldEditorParent());
		genFactbase.getDescriptionControl(getFieldEditorParent()).addListener(SWT.Selection, new Listener() {
			@Override
			public void handleEvent(Event event) {
				metaPred.setEnabled(genFactbase.getBooleanValue(), getFieldEditorParent());
			}
		});
		addField(genFactbase);
		addField(metaPred);
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
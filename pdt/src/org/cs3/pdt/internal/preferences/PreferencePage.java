package org.cs3.pdt.internal.preferences;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.eclipse.jface.preference.DirectoryFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
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

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(PDTPlugin.getDefault().getPreferenceStore());
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	@Override
	public void createFieldEditors() {
	
//		// When i open a file in the prolog editor that does not belong to
//		// a prolog project, ask if i want to add the prolog nature.
//		addField(new RadioGroupFieldEditor(PDT.PREF_ADD_NATURE_ON_OPEN, "Automatically add Prolog Nature when opening pl files", 4,
//				new String[][] { { "always", MessageDialogWithToggle.ALWAYS }, { "never", MessageDialogWithToggle.NEVER },
//						{ "ask", MessageDialogWithToggle.PROMPT } }, getFieldEditorParent(), true));
//
//		// When i consult a prolog file, but the active console view is not
//		// connected to the default runtime
//		// of the respective prolog project, should i switch to the default
//		// runtime first?
//		addField(new RadioGroupFieldEditor(PDT.PREF_SWITCH_TO_DEFAULT_PIF, "Switch to default runtime before consulting", 4,
//				new String[][] { { "always", MessageDialogWithToggle.ALWAYS }, { "never", MessageDialogWithToggle.NEVER },
//						{ "ask", MessageDialogWithToggle.PROMPT } }, getFieldEditorParent(), true));
//		
		
		// Determines the verbosity of the debug log file.			
		RadioGroupFieldEditor rgfe_level = new RadioGroupFieldEditor(PDT.PREF_DEBUG_LEVEL, "Debug Level", 5, new String[][] {
				{ "none", "NONE" }, { "error", "ERROR" }, { "warning", "WARNING" }, { "info", "INFO" }, { "debug", "DEBUG" } },
				getFieldEditorParent(), true);
		addField(rgfe_level);

		RadioGroupFieldEditor rgfe_output = new RadioGroupFieldEditor(PDT.PREF_DEBUG_OUTPUT_TO, "Debug Output to", 3, new String[][] {
				{ "logfile", "LOGFILE" }, { "console", "CONSOLE" } }, getFieldEditorParent(), true);
		addField(rgfe_output);

		// A file to which debug output of the PDT will be writen
		addField(new DirectoryFieldEditor(PDT.PREF_CLIENT_LOG_FILE_DIR, "Log file location", getFieldEditorParent()));		

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

}
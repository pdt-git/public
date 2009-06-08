package org.cs3.pdt.console.preferences;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pl.common.Debug;
import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.FileFieldEditor;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.widgets.Composite;
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

public class PreferencePageMain extends FieldEditorPreferencePage implements IWorkbenchPreferencePage {

	
	public PreferencePageMain() {
		super(GRID);
		setPreferenceStore(PrologConsolePlugin.getDefault().getPreferenceStore());
		setDescription("Preferences for the Prolog Console");
	}
		
	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	public void createFieldEditors() {
		
		
		
		//If enabled, the enter key sends a semicolon(';') when\n
		//while the console is in 'get_single_char/1'-mode, \n
		//e.g., when backtracking over the solutions to a goal.		
		addField(new BooleanFieldEditor(PreferenceConstants.PREF_SHOW_HIDDEN_SUBSCRIPTIONS,"Use Enter Key for backtracking",getFieldEditorParent()));
		
		//When enabled, the console view will be able to detect\n
		//whether the user input stream is read from via get_single_char/1
		//(e.g. when backtracking through query results).\n
		//In those situations, the console view will emulate the
		//unbuffered behaviour of the SWI-Prolog default terminal/plwin
		//interface. This works just fine in most cases, but there have been 
		//reports of problems when using Edinburgh-style io predicates.\n
		//If you get unexpected io behaviour from 
		//your application, disabling this flag may help.
		addField(new BooleanFieldEditor(PreferenceConstants.PREF_ENABLE_CONSOLE_VOODOO,"intercept get_single_char/1 calls",getFieldEditorParent()));

		//The Prolog Console uses this to save its command history.\n
		//Just leave it empty if you do not want the command history to be persistent.
		addField(new FileFieldEditorWithEnsureFileExists(PreferenceConstants.PREF_CONSOLE_HISTORY_FILE,"History File",getFieldEditorParent()));
		
		//Maximum time in milliseconds to wait for the console server to come up.
		addField(new IntegerFieldEditor(PreferenceConstants.PREF_TIMEOUT,"Connect Timeout",getFieldEditorParent()));
		
		//If this flag is set, processes will be shown in the console even if all subscriptions are marked as invisible.
		addField(new BooleanFieldEditor(PreferenceConstants.PREF_SHOW_HIDDEN_SUBSCRIPTIONS,"Show Hidden Processes",getFieldEditorParent()));
		
		//comma-separated list of trackers the console does follow
		StringFieldEditor sfe = new StringFieldEditor(PreferenceConstants.PREF_CONTEXT_TRACKERS,"Active context trackers",getFieldEditorParent());
		addField(sfe);
		// Disabled, because Lukas set it in his implementation to invisible
		sfe.setEnabled(false, getFieldEditorParent()); 
		
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}
	
	
	
	class FileFieldEditorWithEnsureFileExists extends FileFieldEditor{

		

		public FileFieldEditorWithEnsureFileExists() {
			super();
		}

		public FileFieldEditorWithEnsureFileExists(String name, String labelText, boolean enforceAbsolute, Composite parent) {
			super(name, labelText, enforceAbsolute, parent);
		}

		public FileFieldEditorWithEnsureFileExists(String name, String labelText, boolean enforceAbsolute, int validationStrategy, Composite parent) {
			super(name, labelText, enforceAbsolute, validationStrategy, parent);
		}

		public FileFieldEditorWithEnsureFileExists(String name, String labelText, Composite parent) {
			super(name, labelText, parent);
		}
		
		@Override
		protected boolean checkState() {
			return ensureFileExists();// && super.checkState();
		}
		
		private boolean ensureFileExists() {
			
			String msg = null;
			String value = getTextControl().getText();
			
			if (value == null) {
				msg = "History File must not be null";
			}
			if (value.length() == 0) {
				msg = "History File must not be empty";
			}
			File f = new File(value);
			if (!f.isAbsolute()) {
				msg = "History File must be an absolute path";
			}
			if (f.isDirectory()) {
				msg = "History File exists, but is a directory";
			}
			if (!f.exists()) {
				try {
					if (!f.createNewFile()) {

					}
				} catch (IOException e) {
					Debug.report(e);
					msg = "could not create History File";
				}

			}
			if (!f.canWrite()) {
				msg = "History File exists, but is not writable";
			}
			
			
			if (msg != null) { // error
		        showErrorMessage(msg);
		        return false;
			}

	        // OK!
	        clearErrorMessage();
	        return true;
		}
		
	
	}
	
	


}
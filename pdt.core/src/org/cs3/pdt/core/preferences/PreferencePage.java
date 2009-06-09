package org.cs3.pdt.core.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;

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
		setPreferenceStore(PDTCorePlugin.getDefault().getPreferenceStore());
		setDescription("Preferences for the PDT Core");
	}

	/**
	 * Creates the field editors. Field editors are abstractions of the common
	 * GUI blocks needed to manipulate various types of preferences. Each field
	 * editor knows how to save and restore itself.
	 */
	public void createFieldEditors() {
		
		//The default value for the source path of prolog projects.
		addField(new StringFieldEditor(PDTCore.PREF_SOURCE_PATH_DEFAULT, "Default Source Path", getFieldEditorParent()));
		
		//The default value for the Metadata PrologInterface property of prolog projects.
		addField(new StringFieldEditor(PDTCore.PREF_METADATA_PIF_KEY_DEFAULT, "Default Meta Data PrologInterface", getFieldEditorParent()));
	
		//The default value for the Runtime PrologInterface property of prolog projects.
		addField(new StringFieldEditor(PDTCore.PREF_RUNTIME_PIF_KEY_DEFAULT, "Default Runtime PrologInterface", getFieldEditorParent()));
		
		//If true, character offsets read by the prolog core will be interpreted as 
		//logical offsets (e.g. windows line-endings counting as a single character), and 
		//will be converted to physical offsets by the ui.
		addField(new BooleanFieldEditor(PDTCore.PREF_CONVERT_CHARACTER_OFFSETS, "Convert character offsets", getFieldEditorParent()));
		
		//If this flag is set, the PDT will automaticaly (re-)consult any source file,
		//unless it is explicitly exluded from Auto-Consult. Note that this is an experimental
		//feature and defaults to \"false\" for 0.1.x
		addField(new BooleanFieldEditor(PDTCore.PREF_AUTO_CONSULT, "Enable Auto-Consult (EXPERIMENTAL)", getFieldEditorParent()));

		//If this flag is set, the PDT will ignore files that are marked as hidden when looking up
		//predicates and the like. For example, the PDT marks all of its own source code libraries as hidden.
		//Enabling this flag is usefull if you want to edit different versions of the PDT source
		//files than the once the PDT is currently using itself.
		addField(new BooleanFieldEditor(PDTCore.PREF_IGNORE_HIDDEN_LIBS, "Ignore Hidden Libraries", getFieldEditorParent()));		

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.eclipse.ui.IWorkbenchPreferencePage#init(org.eclipse.ui.IWorkbench)
	 */
	public void init(IWorkbench workbench) {
	}

}
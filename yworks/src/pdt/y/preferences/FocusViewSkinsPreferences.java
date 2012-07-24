/* $LICENSE_MSG$ */

package pdt.y.preferences;

import org.eclipse.ui.IWorkbench;

import pdt.y.preferences.controls.FocusViewSkinsEditor;

/**
 * This class represents a preference page that
 * is contributed to the Preferences dialog. By 
 * subclassing <samp>FieldEditorPreferencePage</samp>, we
 * can use the field support built into JFace that allows
 * us to create a page that is small and knows how to 
 * save, restore and apply itself.
 * <p>
 * This page is used to modify preferences only. They
 * are stored in the preference store that belongs to
 * the main plug-in class. That way, preferences can
 * be accessed directly via the preference store.
 */

public class FocusViewSkinsPreferences
	extends PreferencePageBase {

	public FocusViewSkinsPreferences() {
		setDescription("Preferences for the PDT Focus View Plugin");
		
		noDefaultAndApplyButton();
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
	public void createFieldEditors() {
		
		addField(new FocusViewSkinsEditor(getFieldEditorParent()));
	}

	@Override
	public void init(IWorkbench workbench) {
	}
}


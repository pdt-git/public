package pdt.y.preferences;

import org.eclipse.jface.preference.*;
import org.eclipse.ui.IWorkbenchPreferencePage;
import org.eclipse.ui.IWorkbench;

import pdt.y.main.PluginActivator;

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

public class PreferencePage
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	public PreferencePage() {
		super(GRID);
		setPreferenceStore(PluginActivator.getDefault().getPreferenceStore());
		setDescription("Preferences for the PDT Focus View Plugin");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		
//		addField(new DirectoryFieldEditor(PreferenceConstants.P_PATH, 
//				"&Directory preference:", getFieldEditorParent()));
//		addField(
//			new BooleanFieldEditor(
//				PreferenceConstants.P_BOOLEAN,
//				"&An example of a boolean preference",
//				getFieldEditorParent()));
//		addField(
//		new StringFieldEditor(PreferenceConstants.P_STRING, "A &text preference:", getFieldEditorParent()));

		addField(new RadioGroupFieldEditor(
				PreferenceConstants.P_UPDATE_MODE,
			"Update mode",
			1,
			new String[][] { { "&Manual (Refresh button)", PreferenceConstants.P_UPDATE_MODE_MANUAL }, {
				"&Automatic", PreferenceConstants.P_UPDATE_MODE_AUTOMATIC }
		}, getFieldEditorParent()));
	}

	public void init(IWorkbench workbench) {
	}
	
	private static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}

	public static boolean isAutomaticUpdate() {
		String mode = getCurrentPreferences().getString(PreferenceConstants.P_UPDATE_MODE);
		return mode.equals(PreferenceConstants.P_UPDATE_MODE_AUTOMATIC);
	}
}
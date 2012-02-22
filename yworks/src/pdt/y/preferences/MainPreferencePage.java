package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.SHOW_TOOLTIPS;
import static pdt.y.preferences.PreferenceConstants.UPDATE_MODE;
import static pdt.y.preferences.PreferenceConstants.UPDATE_MODE_AUTOMATIC;
import static pdt.y.preferences.PreferenceConstants.UPDATE_MODE_MANUAL;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;

import pdt.y.main.PluginActivator;
import pdt.y.preferences.controls.PreferencesManagementFieldEditor;

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

public class MainPreferencePage
	extends PreferencePageBase {

	public MainPreferencePage() {
		setDescription("Preferences for the PDT Focus View Plugin");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		
		addField(new RadioGroupFieldEditor(UPDATE_MODE,
				"Update mode",
				1,
				new String[][] { { "&Manual (Refresh button)", UPDATE_MODE_MANUAL }, 
								 { "&Automatic", UPDATE_MODE_AUTOMATIC }
				}, getFieldEditorParent()));
		
		addField(new BooleanFieldEditor(SHOW_TOOLTIPS, "Show tooltips", getFieldEditorParent()));
		
		addField(new PreferencesManagementFieldEditor(getFieldEditorParent()));
	}

	public void init(IWorkbench workbench) {
	}
	
	private static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}

	public static boolean isAutomaticUpdate() {
		String mode = getCurrentPreferences().getString(UPDATE_MODE);
		return mode.equals(UPDATE_MODE_AUTOMATIC);
	}
	
	public static boolean isShowToolTip() {
		return getCurrentPreferences().getBoolean(SHOW_TOOLTIPS);
	}
}
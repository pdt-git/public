package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.NAME_CROPPING;
import static pdt.y.preferences.PreferenceConstants.NAME_CROPPING_BRACKET;
import static pdt.y.preferences.PreferenceConstants.NAME_CROPPING_MIDDLE;
import static pdt.y.preferences.PreferenceConstants.NAME_CROPPING_POSTFIX;
import static pdt.y.preferences.PreferenceConstants.NAME_CROPPING_PREFIX;

import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.ui.IWorkbench;

import pdt.y.main.PluginActivator;
import pdt.y.preferences.controls.NodeSizeRadioGroupFieldEditor;

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

public class PredicateLayoutPreferences
	extends PreferencePageBase {
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		addField(new RadioGroupFieldEditor(
			NAME_CROPPING, "Name cropping", 2,
			new String[][] { 
				{ "P&refix                ", NAME_CROPPING_PREFIX },
				{ "P&ostfix               ", NAME_CROPPING_POSTFIX },
				{ "&Bracket               ", NAME_CROPPING_BRACKET }, 
				{ "&Middle                ", NAME_CROPPING_MIDDLE }
			}, getFieldEditorParent()));
		
		addField(new NodeSizeRadioGroupFieldEditor(getFieldEditorParent()));
	}
	
	public void init(IWorkbench workbench) {
	}
	
	public static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static String getNameCroppingConfiguration() {
		return getCurrentPreferences().getString(PreferenceConstants.NAME_CROPPING);
	}
	
	public static String getNodeSizePreference() {
		return getCurrentPreferences().getString(PreferenceConstants.NODE_SIZE);
	}
	
	public static String getLayoutPreference() {
		return getCurrentPreferences().getString(PreferenceConstants.LAYOUT);
	}
	
	public static void setLayoutPreference(String value) {
		getCurrentPreferences().setValue(PreferenceConstants.LAYOUT, value);
	}
}
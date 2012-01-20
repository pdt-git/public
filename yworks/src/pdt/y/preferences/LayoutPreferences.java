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

public class LayoutPreferences
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	public LayoutPreferences() {
		super(GRID);
		setPreferenceStore(PluginActivator.getDefault().getPreferenceStore());
		//setDescription("Layout preferences");
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		addField(new RadioGroupFieldEditor(
			PreferenceConstants.P_NAME_CROPPING,
			"Name cropping",
			2,
			new String[][] { 
				{ "P&refix                ", PreferenceConstants.P_NAME_CROPPING_PREFIX },
				{ "P&ostfix               ", PreferenceConstants.P_NAME_CROPPING_POSTFIX },
				{ "&Bracket               ", PreferenceConstants.P_NAME_CROPPING_BRACKET }, 
				{ "&Middle                ", PreferenceConstants.P_NAME_CROPPING_MIDDLE }
				
		}, getFieldEditorParent()));
	}
	
	public void init(IWorkbench workbench) {
	}
	
	private static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static String getNameCroppingConfiguration() {
		return getCurrentPreferences().getString(PreferenceConstants.P_NAME_CROPPING);
	}
}
/* $LICENSE_MSG$ */

package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.APPEARANCE_LINE_COLOR;

import java.awt.Color;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
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

public class EdgeAppearancePreferences
	extends PreferencePageBase {
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	@Override
	public void createFieldEditors() {
		addField(new ColorFieldEditor(APPEARANCE_LINE_COLOR, "&Line Color    ", wrap(getFieldEditorParent())));
	}

	@Override
	public void init(IWorkbench workbench) {
	}
	
	public static IPreferenceStore getCurrentPreferences() {
		return PluginActivator.getDefault().getPreferenceStore();
	}
	
	public static Color getColor(String preferenceName) {
		String value = getCurrentPreferences().getString(preferenceName);
		RGB color = StringConverter.asRGB(value);
		return new Color(color.red, color.green, color.blue);
	}
	public static Color getLineColor() {
		return getColor(APPEARANCE_LINE_COLOR);
	}
}


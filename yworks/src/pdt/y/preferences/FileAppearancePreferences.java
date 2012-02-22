package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.APPEARANCE_FILE_HEADER_COLOR;
import static pdt.y.preferences.PreferenceConstants.APPEARANCE_MODULE_FILE_BACKGROUND_COLOR;
import static pdt.y.preferences.PreferenceConstants.APPEARANCE_MODULE_HEADER_COLOR;

import java.awt.Color;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

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
public class FileAppearancePreferences
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {
	
	public FileAppearancePreferences() {
		super(GRID);
		setPreferenceStore(PluginActivator.getDefault().getPreferenceStore());
	}
	
	/**
	 * Creates the field editors. Field editors are abstractions of
	 * the common GUI blocks needed to manipulate various types
	 * of preferences. Each field editor knows how to save and
	 * restore itself.
	 */
	public void createFieldEditors() {
		addField(new ColorFieldEditor(APPEARANCE_MODULE_FILE_BACKGROUND_COLOR, "File Background Color", getFieldEditorParent()));
		addField(new ColorFieldEditor(APPEARANCE_MODULE_HEADER_COLOR, "Module File Header Color", getFieldEditorParent()));
		addField(new ColorFieldEditor(APPEARANCE_FILE_HEADER_COLOR, "Non Module File Header Color", getFieldEditorParent()));
	}

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
	
	public static Color getFileHeaderColor() {
		return getColor(APPEARANCE_FILE_HEADER_COLOR);
	}
	
	public static Color getModuleHeaderColor() {
		return getColor(APPEARANCE_MODULE_HEADER_COLOR);
	}
	
	public static Color getModuleFileBackgroundColor() {
		return getColor(APPEARANCE_MODULE_FILE_BACKGROUND_COLOR);
	}
}
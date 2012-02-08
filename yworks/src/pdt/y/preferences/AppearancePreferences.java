package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.*;

import java.awt.Color;

import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import pdt.y.main.PluginActivator;
import y.view.LineType;

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

public class AppearancePreferences
	extends FieldEditorPreferencePage
	implements IWorkbenchPreferencePage {

	private static final LineType[] s_lineTypes = {
		LineType.LINE_2,
		LineType.DASHED_2,
		LineType.DOTTED_2,
		LineType.DASHED_DOTTED_2
	};
	
	public AppearancePreferences() {
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
		
		String[][] lineWidths = new String[][] { 
				{ "Thin      ", Float.toString(APPEARANCE_LINE_WIDTH_THIN) },
				{ "Normal    ", Float.toString(APPEARANCE_LINE_WIDTH_NORMAL) },
				{ "Thick", Float.toString(APPEARANCE_LINE_WIDTH_THICK) }, 
			};
		
		String[][] lineTypes = new String[][] { 
				{ "Solid      ", Integer.toString(APPEARANCE_BORDER_STYLE_SOLID) },
				{ "Dashed     ", Integer.toString(APPEARANCE_BORDER_STYLE_DASHED) },
				{ "Dotted     ", Integer.toString(APPEARANCE_BORDER_STYLE_DOTTED) },
				{ "Dashed Dotted", Integer.toString(APPEARANCE_BORDER_STYLE_DASHED_DOTTED) }, 
			};
		
		addField(new ColorFieldEditor(APPEARANCE_FILE_HEADER_COLOR, "File Header Color", getFieldEditorParent()));
		addField(new ColorFieldEditor(APPEARANCE_MODULE_HEADER_COLOR, "Module Header Color", getFieldEditorParent()));
		
		addField(new ColorFieldEditor(APPEARANCE_MODULE_FILE_BACKGROUND_COLOR, "Module and File Background Color", getFieldEditorParent()));
		
		addField(new ColorFieldEditor(APPEARANCE_PREDICATE_COLOR, "Predicate Background Color", getFieldEditorParent()));
		addField(new ColorFieldEditor(APPEARANCE_EXPORTED_PREDICATE_COLOR, "Exported Predicate Background Color", getFieldEditorParent()));
		
		addField(new ColorFieldEditor(APPEARANCE_BORDER_COLOR, "Border Color", getFieldEditorParent()));
		addField(new ColorFieldEditor(APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR, "Unused Predicate Border Color", getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(APPEARANCE_BORDER_WIDTH, "Border Width", 3, 
				lineWidths, getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(APPEARANCE_BORDER_STYLE, "Border Style", 4, 
				lineTypes, getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE, "Dynamic Predicate Border Style", 4, 
				lineTypes, getFieldEditorParent()));
		
		addField(new ColorFieldEditor(APPEARANCE_LINE_COLOR, "Line Color", getFieldEditorParent()));
		
		addField(new RadioGroupFieldEditor(APPEARANCE_LINE_WIDTH, "Line Width", 3, 
				lineWidths, getFieldEditorParent()));
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
	
	public static Color getPredicateColor() {
		return getColor(APPEARANCE_PREDICATE_COLOR);
	}
	
	public static Color getExportedPredicateColor() {
		return getColor(APPEARANCE_EXPORTED_PREDICATE_COLOR);
	}
	
	public static Color getBorderColor() {
		return getColor(APPEARANCE_BORDER_COLOR);
	}
	
	public static Color getUnusedPredicateBorderColor() {
		return getColor(APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR);
	}
	
	public static float getBorderWidth() {
		return getCurrentPreferences().getFloat(APPEARANCE_BORDER_WIDTH);
	}
	
	public static LineType getBorderStyle() {
		int i = getCurrentPreferences().getInt(APPEARANCE_BORDER_STYLE);
		return s_lineTypes[i];
	}
	
	public static LineType getDynamicPredicateBorderStyle() {
		int i = getCurrentPreferences().getInt(APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE);
		return s_lineTypes[i];
	}
	
	public static Color getLineColor() {
		return getColor(APPEARANCE_LINE_COLOR);
	}
	
	public static float getLineWidth() {
		return getCurrentPreferences().getFloat(APPEARANCE_LINE_WIDTH);
	}
}
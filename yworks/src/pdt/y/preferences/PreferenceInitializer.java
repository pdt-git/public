package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.*;

import java.awt.Color;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.StringConverter;
import org.eclipse.swt.graphics.RGB;

import pdt.y.main.PluginActivator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	public void initializeDefaultPreferences() {
		IPreferenceStore store = PluginActivator.getDefault().getPreferenceStore();
		
		store.setDefault(UPDATE_MODE, UPDATE_MODE_MANUAL);
		store.setDefault(NAME_CROPPING, NAME_CROPPING_PREFIX);
		store.setDefault(NODE_SIZE, NODE_SIZE_MEDIAN);
		store.setDefault(NODE_SIZE_FIXED_HEIGHT, 40);
		store.setDefault(NODE_SIZE_FIXED_WIDTH, 100);
		store.setDefault(LAYOUT, LAYOUT_HIERARCHY);
		store.setDefault(APPEARANCE_FILE_HEADER_COLOR, getColorString(Color.WHITE));
		store.setDefault(APPEARANCE_MODULE_HEADER_COLOR, getColorString(new Color(203, 215, 226)));
		store.setDefault(APPEARANCE_MODULE_FILE_BACKGROUND_COLOR, getColorString(new Color(240, 240, 240)));
		store.setDefault(APPEARANCE_PREDICATE_COLOR, getColorString(Color.YELLOW));
		store.setDefault(APPEARANCE_EXPORTED_PREDICATE_COLOR, getColorString(Color.GREEN));
		store.setDefault(APPEARANCE_BORDER_COLOR, getColorString(Color.BLACK));
		store.setDefault(APPEARANCE_UNUSED_PREDICATE_BORDER_COLOR, getColorString(Color.RED));
		store.setDefault(APPEARANCE_BORDER_WIDTH, APPEARANCE_LINE_WIDTH_THIN);
		store.setDefault(APPEARANCE_BORDER_STYLE, APPEARANCE_BORDER_STYLE_SOLID);
		store.setDefault(APPEARANCE_DYNAMIC_PREDICATE_BORDER_STYLE, APPEARANCE_BORDER_STYLE_DASHED_DOTTED);
		store.setDefault(APPEARANCE_LINE_COLOR, getColorString(Color.DARK_GRAY));
		store.setDefault(APPEARANCE_LINE_WIDTH, APPEARANCE_LINE_WIDTH_THIN);
	}

	private String getColorString(Color color) {
		RGB rgb = new RGB(color.getRed(), color.getGreen(), color.getBlue());
		return StringConverter.asString(rgb);
	}

}

package pdt.y.preferences;

import static pdt.y.preferences.PreferenceConstants.*;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import pdt.y.main.PluginActivator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	public void initializeDefaultPreferences() {
		IPreferenceStore store = PluginActivator.getDefault().getPreferenceStore();
		
		store.setDefault(P_UPDATE_MODE, P_UPDATE_MODE_MANUAL);
		store.setDefault(P_NAME_CROPPING, P_NAME_CROPPING_PREFIX);
		store.setDefault(P_NODE_SIZE, P_NODE_SIZE_MEDIAN);
		store.setDefault(P_NODE_SIZE_FIXED_HEIGHT, 40);
		store.setDefault(P_NODE_SIZE_FIXED_WIDTH, 100);
		store.setDefault(LAYOUT, LAYOUT_HIERARCHY);
	}

}

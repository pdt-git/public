package pdt.y.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import pdt.y.main.PluginActivator;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	public void initializeDefaultPreferences() {
		IPreferenceStore store = PluginActivator.getDefault().getPreferenceStore();
		
		store.setDefault(PreferenceConstants.P_UPDATE_MODE, PreferenceConstants.P_UPDATE_MODE_MANUAL);
		store.setDefault(PreferenceConstants.P_NAME_CROPPING, PreferenceConstants.P_NAME_CROPPING_PREFIX);
	}

}

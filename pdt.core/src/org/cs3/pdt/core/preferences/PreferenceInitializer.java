package org.cs3.pdt.core.preferences;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		IPreferenceStore store = PDTCorePlugin.getDefault().getPreferenceStore();

		store.setDefault(PDTCore.PREF_SOURCE_PATH_DEFAULT, "/");
		store.setDefault(PDTCore.PREF_METADATA_PIF_KEY_DEFAULT, "%project%-meta");
		store.setDefault(PDTCore.PREF_RUNTIME_PIF_KEY_DEFAULT, "%project%-PDT");
		store.setDefault(PDTCore.PREF_CONVERT_CHARACTER_OFFSETS, true);
		store.setDefault(PDTCore.PREF_AUTO_CONSULT, false);
		store.setDefault(PDTCore.PREF_IGNORE_HIDDEN_LIBS, false);
		
		
		
	}

}

package org.cs3.pdt.runtime.pifcom.preferences;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {

		IPreferenceStore store = PrologRuntimePlugin.getDefault().getPreferenceStore();
			
		store.setDefault(PIFComConstants.PIF_PORT_TCP, 9944);
		store.setDefault(PIFComConstants.PIF_PORT_UDP, 9944);
		
		
	}

}

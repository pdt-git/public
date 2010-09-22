package org.cs3.pdt.preferences;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;

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
	@Override
	public void initializeDefaultPreferences() {
		IPreferenceStore store = PDTPlugin.getDefault().getPreferenceStore();
		
		String location = "";
		try {
			location = getLocation();
		} catch (IOException e) {
			Debug.report(e);
			Debug.error("Could not find plugin installation dir.");
		}

		store.setDefault(PDT.PREF_DEBUG_LEVEL, "WARNING");
		store.setDefault(PDT.PREF_DEBUG_OUTPUT_TO,"LOGFILE");
		store.setDefault(PDT.PREF_CLIENT_LOG_FILE_DIR, location);
		store.setDefault(PDT.PREF_ADD_NATURE_ON_OPEN, MessageDialogWithToggle.PROMPT);
		store.setDefault(PDT.PREF_SWITCH_TO_DEFAULT_PIF, MessageDialogWithToggle.PROMPT);
		store.setDefault(PDT.PREF_OUTLINE_FILTERS,"hide_subterms" );
		store.setDefault(PDT.PREF_OUTLINE_SORT, false);
		
		

	}

	private String getLocation() throws IOException {
		URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
		String location = null;
		location = new File(Platform.asLocalURL(url).getFile()).getAbsolutePath();
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}

}

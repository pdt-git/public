package org.cs3.pdt.preferences;

import java.io.IOException;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.PDTColors;
import org.cs3.pl.common.Debug;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;

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
		
		initializeDefaultPreferences_FontAndColor(store);		
	}

	private String getLocation() throws IOException {
		return PDTPlugin.getDefault().getStateLocation().toOSString();
//		URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
//		String location = null;
//		location = new File(FileLocator.toFileURL(url).getFile()).getAbsolutePath();
//		if (location.charAt(location.length() - 1) == File.separatorChar)
//			location = location.substring(0, location.length() - 1);
//		return location;
	}

	private void initializeDefaultPreferences_FontAndColor(IPreferenceStore store){			
		PreferenceConverter.setDefault(store, PDTColors.PREF_BACKGROUND, PDTColors.BACKGROUND);
		PreferenceConverter.setDefault(store, PDTColors.PREF_DEFAULT, PDTColors.DEFAULT);
		PreferenceConverter.setDefault(store, PDTColors.PREF_STRING, PDTColors.STRING);
		PreferenceConverter.setDefault(store, PDTColors.PREF_COMMENT, PDTColors.COMMENT);		
		PreferenceConverter.setDefault(store, PDTColors.PREF_VARIABLE, PDTColors.VARIABLE);
		PreferenceConverter.setDefault(store, PDTColors.PREF_UNDEFINED, PDTColors.UNDEFINED);
		PreferenceConverter.setDefault(store, PDTColors.PREF_BUILTIN, PDTColors.BUILTIN);
		PreferenceConverter.setDefault(store, PDTColors.PREF_DYNAMIC, PDTColors.DYNAMIC);
		PreferenceConverter.setDefault(store, PDTColors.PREF_TRANSPARENT, PDTColors.TRANSPARENT);
		PreferenceConverter.setDefault(store, PDTColors.PREF_META, PDTColors.META);
	}

	
}

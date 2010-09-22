package org.cs3.pdt.runtime.socket.preferences;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.Debug;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
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
		
		PrologRuntimeUIPlugin plugin = PrologRuntimeUIPlugin.getDefault();
		IPreferenceStore store = plugin.getPreferenceStore();
		store.setDefault(SocketPrologInterfacePreferences.PREF_USE_POOL, "true");
		store.setDefault(SocketPrologInterfacePreferences.PREF_CREATE_SERVER_LOGS, false);
		store.setDefault(SocketPrologInterfacePreferences.PREF_PORT, 9944);
		store.setDefault(SocketPrologInterfacePreferences.PREF_HIDE_PLWIN, true);
		
		
		String location = "";
		try {
			location = getLocation();
		} catch (IOException e) {
			Debug.report(e);
			Debug.error("Could not find plugin installation dir.");
		}
		store.setDefault(SocketPrologInterfacePreferences.PREF_SERVER_LOGDIR,location);
	}

	private String getLocation() throws IOException {
		URL url = PrologRuntimeUIPlugin.getDefault().getBundle().getEntry("/");
		String location = getAbsolutePathForURL(url);
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}

	private String getAbsolutePathForURL(URL url) throws IOException {
		String fileName = FileLocator.toFileURL(url).getFile();
		File file = new File(fileName);
		String absolutePath = file.getAbsolutePath();
		return absolutePath;
	}
	
}

package org.cs3.pdt.runtime.socket.preferences;

import java.io.File;
import java.io.IOException;
import java.net.URL;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.internal.socket.SocketPrologInterface;
import org.eclipse.core.runtime.Platform;
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
	public void initializeDefaultPreferences() {
		
		PrologRuntimePlugin plugin = PrologRuntimePlugin.getDefault();
		IPreferenceStore store = plugin.getPreferenceStore();
		
//		store.setDefault(ProcessKiller.PREF_KILLCOMMAND, ProcessKiller.guessKillCommandName());
		store.setDefault(SocketPrologInterface.PREF_USE_POOL, guessUsePool());
		store.setDefault(SocketPrologInterface.PREF_CREATE_SERVER_LOGS, false);
		store.setDefault(SocketPrologInterface.PREF_PORT, 9944);
		store.setDefault(SocketPrologInterface.PREF_HIDE_PLWIN, true);
		
		
		String location = "";
		try {
			location = getLocation();
		} catch (IOException e) {
			Debug.report(e);
			Debug.error("Could not find plugin installation dir.");
		}
		store.setDefault(SocketPrologInterface.PREF_SERVER_LOGDIR,location);
	}

	/**
	 * @return
	 */
	public static String guessUsePool() {
		return "true";
	}

	private String getLocation() throws IOException {
		URL url = PrologRuntimePlugin.getDefault().getBundle().getEntry("/");
		String location = null;
		location = new File(Platform.asLocalURL(url).getFile()).getAbsolutePath();
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}
	
}

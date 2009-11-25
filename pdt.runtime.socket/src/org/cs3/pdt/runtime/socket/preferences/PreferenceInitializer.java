package org.cs3.pdt.runtime.socket.preferences;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.prolog.internal.socket.SocketPrologInterface;
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
	}

	/**
	 * @return
	 */
	public static String guessUsePool() {
		return "true";
	}

	
}

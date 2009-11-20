package org.cs3.pdt.runtime.socket.preferences;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.socket.Factory;
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
		
		store.setDefault(SocketPrologInterface.PREF_KILLCOMMAND, guessKillCommandName());
		store.setDefault(SocketPrologInterface.PREF_USE_POOL, guessUsePool());
		store.setDefault(SocketPrologInterface.PREF_CREATE_LOGS, false);
		store.setDefault(SocketPrologInterface.PREF_PORT, 9944);
		store.setDefault(SocketPrologInterface.PREF_HIDE_PLWIN, true);
	}

	/**
	 * @return
	 */
	public static String guessUsePool() {
		return "true";
	}

	public String guessKillCommandName() {
	
		if (Util.isWindows()) {
			try {
				
				
				ResourceFileLocator locator = new DefaultResourceFileLocator(new File( System.getProperty("java.io.tmpdir")));
				PrologRuntimePlugin plugin = PrologRuntimePlugin.getDefault();
//				ResourceFileLocator locator = plugin.getPrologInterfaceFactory().getResourceLocator();
				return locator.resolve(Factory.FKILL_EXE).getCanonicalPath();
//				return getResourceLocator().resolve(Factory.FKILL_EXE).getCanonicalPath();
						
			} catch (IOException e) {
				Debug.report(e);
				return "kill";
			}
		}

		return "kill";

	}
}

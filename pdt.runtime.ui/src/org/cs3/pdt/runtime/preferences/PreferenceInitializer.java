package org.cs3.pdt.runtime.preferences;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.runtime.ui.PrologRuntimeUI;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * Class used to initialize default preference values.
 */
public class PreferenceInitializer extends AbstractPreferenceInitializer {

	protected PrologRuntimeUIPlugin plugin;
	protected IPreferenceStore store;
	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @seeorg.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#
	 * initializeDefaultPreferences()
	 */
	public void initializeDefaultPreferences() {
		plugin = PrologRuntimeUIPlugin.getDefault();
		store = plugin.getPreferenceStore();

		
		store.setDefault(PrologInterface.PREF_FILE_SEARCH_PATH, PrologRuntimePlugin.guessFileSearchPath("pdt.runtime.socket.codebase"));
		
//		store.setDefault(PrologRuntime.PREF_PROLOGIF_IMPLEMENTATION, AbstractPrologInterface.PL_INTERFACE_DEFAULT);
		store.setDefault(PrologRuntimeUI.PREF_PIF_BOOTSTRAP_DIR, System.getProperty("java.io.tmpdir"));
		
		store.setDefault(PrologInterface.PREF_EXECUTABLE, Util.guessExecutableName());
		store.setDefault(PrologInterface.PREF_ENVIRONMENT, Util.guessEnvironmentVariables());
		store.setDefault(PrologInterface.PREF_STANDALONE, "false");
		store.setDefault(PrologInterface.PREF_HOST, "localhost");
		
		store.setDefault(PrologInterface.PREF_TIMEOUT,15000 );
		
		

	}

	
}

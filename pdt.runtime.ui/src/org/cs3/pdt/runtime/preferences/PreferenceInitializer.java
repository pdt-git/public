package org.cs3.pdt.runtime.preferences;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pl.common.Util;
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
	@Override
	public void initializeDefaultPreferences() {
		plugin = PrologRuntimeUIPlugin.getDefault();
		store = plugin.getPreferenceStore();

		// TODO: replace configuration of file_search_path for consult_server with bootstrap contribution, the library manager is an unnecessary complex concept for the purpose
		store.setDefault(PrologRuntime.PREF_FILE_SEARCH_PATH, PrologRuntimePlugin.guessFileSearchPath("pdt.runtime.socket.codebase"));
		
		store.setDefault(PrologRuntime.PREF_INVOCATION, Util.getInvocationCommand());
		store.setDefault(PrologRuntime.PREF_EXECUTABLE, Util.getExecutablePreference());
		store.setDefault(PrologRuntime.PREF_COMMAND_LINE_ARGUMENTS, Util.getStackCommandLineParameters());
		store.setDefault(PrologRuntime.PREF_ADDITIONAL_STARTUP, "");
		store.setDefault(PrologRuntime.PREF_ENVIRONMENT, Util.guessEnvironmentVariables());
		
		store.setDefault(PrologRuntime.PREF_HOST, "localhost");
		
		store.setDefault(PrologRuntime.PREF_TIMEOUT,15000 );
		store.setDefault(PrologRuntime.PREF_PORT, 9944);
		store.setDefault(PrologRuntime.PREF_HIDE_PLWIN, true);
		
		store.setDefault(PrologRuntime.PREF_GENERATE_FACTBASE, false);
		store.setDefault(PrologRuntime.PREF_META_PRED_ANALYSIS, false);
		
		store.setDefault(PrologRuntime.PREF_SERVER_LOGDIR, PrologRuntimeUIPlugin.getDefault().getStateLocation().toOSString());

	}

	
}

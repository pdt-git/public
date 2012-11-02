package org.cs3.prolog.connector.internal.preferences;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologRuntime;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceStore;

public class PreferenceConfiguration {

	private static PreferenceConfiguration instance;
	
	public static PreferenceConfiguration getInstance() {
		if (instance == null) {
			instance = new PreferenceConfiguration();
		}
		return instance;
	}
	
	private PreferenceConfiguration() {
	}
	
	public static final String CONFIGURATION_SWI = "config.swi";
	public static final String CONFIGURATION_SWI_LOGTALK = "config.swi.logtalk";
	public static final String PREF_CONFIGURATION_IDS = "pif.config.ids";
	
	private static final String CONFIG_DEFAULT_ID_PREFIX = "pif.config.default.id.";
	private static final String CONFIG_LABEL_PREFIX = "pif.config.label.";
	
	private static final String CONFIG_ID_PREFIX = "user.config.";
	private static final List<String> defaultConfigs = Arrays.<String>asList(new String[]{CONFIGURATION_SWI, CONFIGURATION_SWI_LOGTALK});

	public static void initializeDefaultPreferences(IPreferenceStore store) {
		store.setDefault(PREF_CONFIGURATION_IDS, CONFIGURATION_SWI + ";" + CONFIGURATION_SWI_LOGTALK);
		
		store.setDefault(CONFIG_DEFAULT_ID_PREFIX + CONFIGURATION_SWI, CONFIGURATION_SWI);
		store.setDefault(CONFIG_DEFAULT_ID_PREFIX + CONFIGURATION_SWI_LOGTALK, CONFIGURATION_SWI_LOGTALK);
		
		store.setDefault(CONFIG_LABEL_PREFIX + CONFIGURATION_SWI, "SWI Prolog");
		store.setDefault(CONFIG_LABEL_PREFIX + CONFIGURATION_SWI_LOGTALK, "SWI Prolog & Logtalk");
	}

	private HashMap<String, PreferenceStore> stores = new HashMap<String, PreferenceStore>();
	
	public PreferenceStore getPreferenceStore(String configuration) {
		if (getConfigurationIds().contains(configuration)) {
			PreferenceStore store = stores.get(configuration);
			if (store == null) {
				store = createStore(configuration);
				stores.put(configuration, store);
			}
			return store;
		} else {
			return null;
		}
	}
	
	private PreferenceStore createStore(String configuration) {
		PreferenceStore store = new PreferenceStore(PrologRuntimeUIPlugin.getDefault().getStateLocation().append(configuration).toString());
		String defaultId = getDefaultId(configuration);
		if (defaultId.equals(CONFIGURATION_SWI)) {
			initWithSWIPreferences(store);
		} else if (defaultId.equals(CONFIGURATION_SWI_LOGTALK)) {
			initWithSWILogtalkPreferences(store);
		} else {
			Debug.error("Invalid default configuration " + defaultId + " of " + configuration);
		}
		try {
			store.load();
		} catch (IOException e) {
		}
		return store;
	}
	
	public String getDefaultId(String configuration) {
		return PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(CONFIG_DEFAULT_ID_PREFIX + configuration);
	}
	
	public String getLabel(String configuration) {
		return PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(CONFIG_LABEL_PREFIX + configuration);
	}

	public List<String> getConfigurationIds() {
		return Arrays.<String>asList(PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(PREF_CONFIGURATION_IDS).split(";"));
	}
	
	public List<String> getDefaultConfigurationIds() {
		return defaultConfigs;
	}
	
	public String newConfigurationId(String defaultConfig, String label){
		if (!defaultConfigs.contains(defaultConfig) || label == null || label.isEmpty()) {
			return null;
		}
		List<String> configIds = getConfigurationIds();
		String lastConfigId = configIds.get(configIds.size() - 1);
		String newConfigId;
		if (lastConfigId.startsWith(CONFIG_ID_PREFIX)) {
			newConfigId = lastConfigId + "0";
		} else {
			newConfigId = CONFIG_ID_PREFIX + "0";
		}
		StringBuffer buf = new StringBuffer();
		for (String configId : configIds) {
			buf.append(configId);
			buf.append(';');
		}
		buf.append(newConfigId);
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(PREF_CONFIGURATION_IDS, buf.toString());
		
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(CONFIG_DEFAULT_ID_PREFIX + newConfigId, defaultConfig);
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(CONFIG_LABEL_PREFIX + newConfigId, label);
		
		return newConfigId;
	}
	
	public void deleteConfiguration(String configuration) {
		List<String> configurationIds = new ArrayList<String>();
		configurationIds.addAll(getConfigurationIds());
		if (configurationIds.contains(configuration) && !defaultConfigs.contains(configuration)) {
			configurationIds.remove(configuration);
		} else {
			return;
		}
		stores.remove(configuration);
		boolean first = true;
		StringBuffer buf = new StringBuffer();
		for (String configurationId : configurationIds) {
			if (!first) {
				buf.append(';');
			} else {
				first = false;
			}
			buf.append(configurationId);
		}
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(PREF_CONFIGURATION_IDS, buf.toString());
	}
	
	public static void initWithSWIPreferences(IPreferenceStore store) {
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

	public static void initWithSWILogtalkPreferences(IPreferenceStore store) {
		store.setDefault(PrologRuntime.PREF_FILE_SEARCH_PATH, PrologRuntimePlugin.guessFileSearchPath("pdt.runtime.socket.codebase"));
		
		store.setDefault(PrologRuntime.PREF_INVOCATION, Util.getInvocationCommand());
		store.setDefault(PrologRuntime.PREF_EXECUTABLE, Util.getExecutablePreference());
		store.setDefault(PrologRuntime.PREF_COMMAND_LINE_ARGUMENTS, Util.getStackCommandLineParameters());
		store.setDefault(PrologRuntime.PREF_ADDITIONAL_STARTUP, Util.getLogtalkStartupFile());
		store.setDefault(PrologRuntime.PREF_ENVIRONMENT, Util.getLogtalkEnvironmentVariables());
		
		store.setDefault(PrologRuntime.PREF_HOST, "localhost");
		
		store.setDefault(PrologRuntime.PREF_TIMEOUT,15000 );
		store.setDefault(PrologRuntime.PREF_PORT, 9944);
		store.setDefault(PrologRuntime.PREF_HIDE_PLWIN, true);
		
		store.setDefault(PrologRuntime.PREF_GENERATE_FACTBASE, false);
		store.setDefault(PrologRuntime.PREF_META_PRED_ANALYSIS, false);
		
		store.setDefault(PrologRuntime.PREF_SERVER_LOGDIR, PrologRuntimeUIPlugin.getDefault().getStateLocation().toOSString());
	}

}

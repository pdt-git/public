/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector.internal.preferences;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

import org.cs3.pdt.connector.PrologRuntimeUI;
import org.cs3.pdt.connector.PrologRuntimeUIPlugin;
import org.cs3.prolog.common.PDTConstants;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.Connector;
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
		configurations = new ArrayList<String>();
		loadConfigurations();
	}
	
	public static final String PREF_CONFIGURATIONS = "pif.configurations";

	private static final String DEFAULT_CONFIGURATION_PREFIX = "pif.configuration.default.";
	
	
	private final List<String> defaultConfigurations = Arrays.<String>asList(new String[]{PrologRuntimeUI.CONFIGURATION_SWI, PrologRuntimeUI.CONFIGURATION_SWI_LOGTALK, PrologRuntimeUI.CONFIGURATION_YAP, PrologRuntimeUI.CONFIGURATION_YAP_LOGTALK});
	private ArrayList<String> configurations;

	public static void initializeDefaultPreferences(IPreferenceStore store) {
		store.setDefault(PREF_CONFIGURATIONS, PrologRuntimeUI.CONFIGURATION_SWI + ";" + PrologRuntimeUI.CONFIGURATION_SWI_LOGTALK + ";" + PrologRuntimeUI.CONFIGURATION_YAP + ";" + PrologRuntimeUI.CONFIGURATION_YAP_LOGTALK);
		
		store.setDefault(DEFAULT_CONFIGURATION_PREFIX + PrologRuntimeUI.CONFIGURATION_SWI, PrologRuntimeUI.CONFIGURATION_SWI);
		store.setDefault(DEFAULT_CONFIGURATION_PREFIX + PrologRuntimeUI.CONFIGURATION_SWI_LOGTALK, PrologRuntimeUI.CONFIGURATION_SWI_LOGTALK);
		store.setDefault(DEFAULT_CONFIGURATION_PREFIX + PrologRuntimeUI.CONFIGURATION_YAP, PrologRuntimeUI.CONFIGURATION_YAP);
		store.setDefault(DEFAULT_CONFIGURATION_PREFIX + PrologRuntimeUI.CONFIGURATION_YAP_LOGTALK, PrologRuntimeUI.CONFIGURATION_YAP_LOGTALK);
	}

	private HashMap<String, PreferenceStore> stores = new HashMap<String, PreferenceStore>();
	
	public PreferenceStore getPreferenceStore(String configuration) {
		if (configuration == null) {
			return null;
		}
		synchronized (configurations) {
			if (!configurations.contains(configuration)) {
				return null;
			}
		}
		PreferenceStore store = stores.get(configuration);
		if (store == null) {
			store = createStore(configuration);
			stores.put(configuration, store);
		}
		return store;
	}
	
	private PreferenceStore createStore(String configuration) {
		PreferenceStore store = new PreferenceStore(getConfigurationFileName(configuration));
		String defaultConfiguration = getDefaultConfiguration(configuration);
		if (defaultConfiguration.equals(PrologRuntimeUI.CONFIGURATION_SWI)) {
			initWithSWIPreferences(store);
		} else if (defaultConfiguration.equals(PrologRuntimeUI.CONFIGURATION_SWI_LOGTALK)) {
			initWithSWILogtalkPreferences(store);
		} else if (defaultConfiguration.equals(PrologRuntimeUI.CONFIGURATION_YAP)) {
			initWithYAPPreferences(store);
		} else if (defaultConfiguration.equals(PrologRuntimeUI.CONFIGURATION_YAP_LOGTALK)) {
			initWithYAPLogtalkPreferences(store);
		} else {
			Debug.error("Invalid default configuration " + defaultConfiguration + " of " + configuration);
		}
		try {
			store.load();
		} catch (IOException e) {
		}
		return store;
	}

	private String getConfigurationFileName(String configuration) {
		return PrologRuntimeUIPlugin.getDefault().getStateLocation().append(configuration).toString();
	}
	
	public String getDefaultConfiguration(String configuration) {
		return PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(DEFAULT_CONFIGURATION_PREFIX + configuration);
	}
	
	public List<String> getConfigurations() {
		List<String> result;
		synchronized (configurations) {
			result = new ArrayList<String>(configurations);
		}
		return result;
	}
	
	public List<String> getDefaultConfigurations() {
		return new ArrayList<String>(defaultConfigurations);
	}
	
	public boolean addConfiguration(String configuration, String defaultConfiguration){
		if (!defaultConfigurations.contains(defaultConfiguration) || configuration == null || configuration.contains(";")) {
			return false;
		}
		synchronized (configurations) {
			if (configurations.contains(configuration)) {
				return false;
			} else {
				configurations.add(configuration);
			}
			saveConfigurations();
		}
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(DEFAULT_CONFIGURATION_PREFIX + configuration, defaultConfiguration);
		return true;
	}
	
	public boolean deleteConfiguration(String configuration) {
		if (defaultConfigurations.contains(configuration)) {
			return false;
		}
		synchronized (configurations) {
			if (!configurations.contains(configuration)) {
				return false;
			}
			configurations.remove(configuration);
			stores.remove(configuration);
		}
		saveConfigurations();
		try {
			new File(getConfigurationFileName(configuration)).delete();
		} catch (Exception e) {}
		return true;
	}
	
	private void loadConfigurations() {
		synchronized (configurations) {
			for (String configurationId : PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(PREF_CONFIGURATIONS).split(";")) {
				configurations.add(configurationId);
			}
		}
	}
	
	private void saveConfigurations() {
		StringBuffer buf = new StringBuffer();
		boolean first = true;
		synchronized (configurations) {
			for (String configuration : configurations) {
				if (!first) {
					buf.append(';');
				} else {
					first = false;
				}
				buf.append(configuration);
			}
		}
		PrologRuntimeUIPlugin.getDefault().getPreferenceStore().setValue(PREF_CONFIGURATIONS, buf.toString());
	}
	
	private static void initPreferences(IPreferenceStore store) {
		store.setDefault(Connector.PREF_INVOCATION, Util.getInvocationCommand());
		store.setDefault(Connector.PREF_COMMAND_LINE_ARGUMENTS, "");
		store.setDefault(Connector.PREF_ADDITIONAL_STARTUP, "");
		store.setDefault(Connector.PREF_ENVIRONMENT, Util.guessEnvironmentVariables());
		
		store.setDefault(Connector.PREF_HOST, "localhost");
		
		store.setDefault(Connector.PREF_TIMEOUT,15000 );
		store.setDefault(Connector.PREF_PORT, 9944);
		store.setDefault(Connector.PREF_HIDE_PLWIN, true);
		
		store.setDefault(Connector.PREF_SERVER_LOGDIR, PrologRuntimeUIPlugin.getDefault().getStateLocation().toOSString());
	}
	
	public static void initWithSWIPreferences(IPreferenceStore store) {
		initPreferences(store);
		store.setDefault(Connector.PREF_EXECUTABLE, Util.getExecutablePreference(PDTConstants.DIALECT_SWI));
	}

	public static void initWithSWILogtalkPreferences(IPreferenceStore store) {
		initWithSWIPreferences(store);

		store.setDefault(Connector.PREF_ADDITIONAL_STARTUP, Util.getLogtalkStartupFile());
		store.setDefault(Connector.PREF_ENVIRONMENT, Util.getLogtalkEnvironmentVariables());
	}

	public static void initWithYAPPreferences(IPreferenceStore store) {
		initPreferences(store);
		store.setDefault(Connector.PREF_EXECUTABLE, Util.getExecutablePreference(PDTConstants.DIALECT_YAP));
	}

	public static void initWithYAPLogtalkPreferences(IPreferenceStore store) {
		initWithYAPPreferences(store);

		store.setDefault(Connector.PREF_ADDITIONAL_STARTUP, Util.getLogtalkStartupFile());
		store.setDefault(Connector.PREF_ENVIRONMENT, Util.getLogtalkEnvironmentVariables());
	}

}

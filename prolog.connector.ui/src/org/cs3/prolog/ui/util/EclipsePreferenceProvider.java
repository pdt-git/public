/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.ui.util;

import org.cs3.prolog.common.PreferenceProvider;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.internal.preferences.PreferenceConfiguration;
import org.cs3.prolog.connector.ui.PrologRuntimeUI;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.eclipse.jface.preference.PreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class EclipsePreferenceProvider implements PreferenceProvider {

	private AbstractUIPlugin plugin;
	private PreferenceStore store;

	public EclipsePreferenceProvider(AbstractUIPlugin plugin, String configurationId) {
		this.plugin = plugin;
		if (configurationId == null) {
			this.store = PreferenceConfiguration.getInstance().getPreferenceStore(PrologRuntimeUIPlugin.getDefault().getPreferenceStore().getString(PrologRuntimeUI.PREF_CONFIGURATION));
		} else {
			this.store = PreferenceConfiguration.getInstance().getPreferenceStore(configurationId);
		}
	}
	@Override
	public String getPreference(String key) {
		return overridePreferenceBySystemProperty(key);
	}
	
	public String overridePreferenceBySystemProperty( String name) {
		String value;
		value = System.getProperty(name);

		if (value != null) {
			Debug.warning("option " + name + " is overridden by system property: " + value);
			return value;
		}
		
		if (store.contains(name)) {
			return store.getString(name);
		} else {
			value = plugin.getPreferenceStore().getString(name);
		}
		
		return value;
	}
}



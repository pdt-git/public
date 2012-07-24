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
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class EclipsePreferenceProvider implements PreferenceProvider {

	private AbstractUIPlugin plugin;

	public EclipsePreferenceProvider(AbstractUIPlugin plugin) {
		this.plugin = plugin;
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
		
		value = plugin.getPreferenceStore().getString(name);
		
		return value;
	}
}



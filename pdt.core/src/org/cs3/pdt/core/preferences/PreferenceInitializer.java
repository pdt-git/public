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

package org.cs3.pdt.core.preferences;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
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
		IPreferenceStore store = PDTCorePlugin.getDefault().getPreferenceStore();

		store.setDefault(PDTCore.PREF_SOURCE_PATH_DEFAULT, "/");
		store.setDefault(PDTCore.PREF_METADATA_PIF_KEY_DEFAULT, "%project%-meta");
		store.setDefault(PDTCore.PREF_RUNTIME_PIF_KEY_DEFAULT, "%project%-PDT");
		store.setDefault(PDTCore.PREF_CONVERT_CHARACTER_OFFSETS, true);
		store.setDefault(PDTCore.PREF_AUTO_CONSULT, false);
		store.setDefault(PDTCore.PREF_IGNORE_HIDDEN_LIBS, false);
		
		
		
	}

}



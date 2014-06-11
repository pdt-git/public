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

package org.cs3.pdt.console.internal.preferences;

import java.io.File;

import org.cs3.pdt.connector.PrologContextTracker;
import org.cs3.pdt.connector.PDTConnectorPlugin;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.widgets.Display;

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
		IPreferenceStore store = PrologConsolePlugin.getDefault().getPreferenceStore();
		
		initializeDefaultPreferences_Main(store);
		initializeDefaultPreferences_FontAndColor(store);		
		
	}
	
	
	private void initializeDefaultPreferences_Main(IPreferenceStore store){
		String historyFile = System.getProperty("user.home") + File.separator + ".prolog_console_history";		
		store.setDefault(PDTConsole.PREF_CONSOLE_HISTORY_FILE,	historyFile);
			
		
		store.setDefault(PDTConsole.PREF_CONTEXT_TRACKERS,	"");
	}
	
	public String getDefaultContextTrackers() {

		PrologContextTracker[] trackers = PDTConnectorPlugin
				.getDefault().getContextTrackerService()
				.getContextTrackers();
		StringBuffer sb = new StringBuffer();
		for (int i = 0; i < trackers.length; i++) {
			PrologContextTracker tracker = trackers[i];
			if (i > 0) {
				sb.append(',');
			}
			sb.append(tracker.getId());
		}
		return sb.toString();
	}
	
	private void initializeDefaultPreferences_FontAndColor(IPreferenceStore store){	

		FontData fd = new FontData("Courier New", 10, SWT.NORMAL);
		PreferenceConverter.setDefault(store, PDTConsole.PREF_CONSOLE_FONT, fd);

		Color color_err = Display.getDefault().getSystemColor(SWT.COLOR_RED);
		Color color_warn = new Color(Display.getDefault(),255,128,50);
		Color color_info = Display.getDefault().getSystemColor(SWT.COLOR_BLUE);
		Color color_dbg = Display.getDefault().getSystemColor(SWT.COLOR_MAGENTA);
		PreferenceConverter.setDefault(store, PDTConsole.PREF_CONSOLE_COLOR_ERROR, color_err.getRGB());
		PreferenceConverter.setDefault(store, PDTConsole.PREF_CONSOLE_COLOR_WARNING, color_warn.getRGB());
		PreferenceConverter.setDefault(store, PDTConsole.PREF_CONSOLE_COLOR_INFO, color_info.getRGB());
		PreferenceConverter.setDefault(store, PDTConsole.PREF_CONSOLE_COLOR_DEBUG, color_dbg.getRGB());

		store.setDefault(PDTConsole.PREF_CONSOLE_COLORS_THREESTARS, true);

	}

}



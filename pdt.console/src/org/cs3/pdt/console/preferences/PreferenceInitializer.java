package org.cs3.pdt.console.preferences;

import java.io.File;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.runtime.ui.PrologContextTracker;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
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
		store.setDefault(PDTConsole.PREF_TIMEOUT, 15000);
		store.setDefault(PDTConsole.PREF_SHOW_HIDDEN_SUBSCRIPTIONS, false);
		store.setDefault(PDTConsole.PREF_ENTER_FOR_BACKTRACKING, false);
		store.setDefault(PDTConsole.PREF_INTERCEPT_GET_SINGLE_CHAR, true);
		
		String historyFile = System.getProperty("user.home") + File.separator + ".prolog_console_history";		
		store.setDefault(PDTConsole.PREF_CONSOLE_HISTORY_FILE,	historyFile);
			
		
		store.setDefault(PDTConsole.PREF_CONTEXT_TRACKERS,	getDefaultContextTrackers());
	}
	
	public String getDefaultContextTrackers() {

		PrologContextTracker[] trackers = PrologRuntimeUIPlugin
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

		store.setDefault(PDTConsole.PREF_CONSOLE_SHOW_COLORS, true);

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

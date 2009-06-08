package org.cs3.pdt.console.preferences.fontcolor;

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
	public void initializeDefaultPreferences() {
		IPreferenceStore store = PrologConsolePlugin.getDefault().getPreferenceStore();

		// public static final Font DEFAULT_CONSOLE_FONT = "pdt.console.font";
		// public static final Boolean DEFAULT_CONSOLE_SHOW_COLORS = true;
		// public static final int DEFAULT_CONSOLE_COLOR_ERROR = SWT.COLOR_RED;
		// public static final int DEFAULT_CONSOLE_COLOR_WARNING =
		// SWT.COLOR_DARK_YELLOW;
		// public static final int DEFAULT_CONSOLE_COLOR_INFO = SWT.COLOR_BLUE;
		// public static final int DEFAULT_CONSOLE_COLOR_DEBUG =
		// SWT.COLOR_MAGENTA;

		FontData fd = new FontData("Courier New", 10, SWT.NORMAL);
		PreferenceConverter.setDefault(store, PreferenceConstants.PREF_CONSOLE_FONT, fd);

		store.setDefault(PreferenceConstants.PREF_CONSOLE_SHOW_COLORS, true);

		Color color_err = Display.getDefault().getSystemColor(SWT.COLOR_RED);
		Color color_warn = new Color(Display.getDefault(),255,128,50);
		Color color_info = Display.getDefault().getSystemColor(SWT.COLOR_BLUE);
		Color color_dbg = Display.getDefault().getSystemColor(SWT.COLOR_MAGENTA);
		PreferenceConverter.setDefault(store, PreferenceConstants.PREF_CONSOLE_COLOR_ERROR, color_err.getRGB());
		PreferenceConverter.setDefault(store, PreferenceConstants.PREF_CONSOLE_COLOR_WARNING, color_warn.getRGB());
		PreferenceConverter.setDefault(store, PreferenceConstants.PREF_CONSOLE_COLOR_INFO, color_info.getRGB());
		PreferenceConverter.setDefault(store, PreferenceConstants.PREF_CONSOLE_COLOR_DEBUG, color_dbg.getRGB());

		store.setDefault(PreferenceConstants.PREF_CONSOLE_COLORS_THREESTARS, true);

	}

}

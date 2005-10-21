/*
 * Created on 28.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.console.internal.preferences;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.ui.util.OptionPreferencePage;

public class PreferencePage extends OptionPreferencePage {

	public PreferencePage() {
		super(PrologConsolePlugin.getDefault().getPreferenceStore(),
				PrologConsolePlugin.getDefault().getOptions(),
				"Preferences for the Prolog Console");

	}

	protected void reconfigure() {
		PrologConsolePlugin.getDefault().reconfigure();

	}

}
package org.cs3.pdt.runtime.internal.preferences;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.OptionPreferencePage;

public class PreferencePage extends OptionPreferencePage {

	public PreferencePage() {
		super(PrologRuntimePlugin.getDefault().getPreferenceStore(), PrologRuntimePlugin
				.getDefault().getOptions(),
				"Preferences for the PDT Plugin");

	}

	protected void reconfigure() {
		PrologRuntimePlugin.getDefault().reconfigure();

	}

}
package org.cs3.pdt.internal.preferences;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.ui.util.OptionPreferencePage;

public class PreferencePage extends OptionPreferencePage {

	public PreferencePage() {
		super(PDTPlugin.getDefault().getPreferenceStore(), PDTPlugin
				.getDefault().getOptions(),
				"Preferences for the PDT Plugin");

	}

	protected void reconfigure() {
		PDTPlugin.getDefault().reconfigure();

	}

}
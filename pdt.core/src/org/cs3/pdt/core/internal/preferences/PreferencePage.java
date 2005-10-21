package org.cs3.pdt.core.internal.preferences;


import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.ui.util.OptionPreferencePage;

public class PreferencePage extends OptionPreferencePage {

	public PreferencePage() {
		super(PDTCorePlugin.getDefault().getPreferenceStore(), PDTCorePlugin
				.getDefault().getOptions(),
				"Preferences for the PDT Core");

	}

	protected void reconfigure() {
		PDTCorePlugin.getDefault().reconfigure();

	}

}
package org.cs3.jtransformer.internal.preferences;

import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pdt.ui.util.OptionPreferencePage;

public class PreferencePage extends OptionPreferencePage {

	public PreferencePage() {
		super(JTransformerPlugin.getDefault().getPreferenceStore(),
				JTransformerPlugin.getDefault().getOptions(),
				"Preferences for the JTransformer Plugin");

	}

	protected void reconfigure() {
		JTransformerPlugin.getDefault().reconfigure();

	}

}
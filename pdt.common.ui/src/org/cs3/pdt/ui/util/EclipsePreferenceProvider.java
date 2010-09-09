package org.cs3.pdt.ui.util;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.PreferenceProvider;
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

package org.cs3.pdt.runtime.internal.preferences;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.OptionPreferencePage;
import org.cs3.pl.common.Option;
import org.cs3.pl.prolog.PrologInterfaceFactory;

public class PreferencePage extends OptionPreferencePage {

	public PreferencePage() {
		super();
		
		PrologRuntimePlugin plugin = PrologRuntimePlugin.getDefault();
		setPreferenceStore(plugin.getPreferenceStore());
		setDescription("Preferences for the Prolog Interface");
		Option[] pluginOptions = plugin.getOptions();
		String fqn = plugin.getPreferenceValue(PrologRuntime.PREF_PIF_IMPLEMENTATION,null);
		PrologInterfaceFactory factory = fqn==null?PrologInterfaceFactory.newInstance():PrologInterfaceFactory.newInstance(fqn);
		Option[] factoryOptions = factory.getOptions();
		Option[] allOptions = new Option[factoryOptions.length+pluginOptions.length];
		System.arraycopy(pluginOptions,0,allOptions,0,pluginOptions.length);
		System.arraycopy(factoryOptions,0,allOptions,pluginOptions.length,factoryOptions.length);
		setOptions(allOptions);
	}

	protected void reconfigure() {
		PrologRuntimePlugin.getDefault().reconfigure();

	}

}
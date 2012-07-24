/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.console;

import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.console.internal.ConsoleReloadExecutor;
import org.cs3.pdt.console.internal.DefaultPrologConsoleService;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.service.IPrologInterfaceService;
import org.cs3.prolog.ui.util.DefaultErrorMessageProvider;
import org.cs3.prolog.ui.util.ErrorMessageProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PrologConsolePlugin extends AbstractUIPlugin implements IStartup {

	// The shared instance.
	private static PrologConsolePlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PrologConsolePlugin getDefault() {
		return plugin;
	}

	public PrologConsolePlugin() {
		super();
		plugin = this;
		try {
			ResourceBundle.getBundle("prg.cs3.pdt.PDTPluginResources");
		} catch (MissingResourceException x) {
		}
	}
	
	@Override
	public void start(BundleContext context) throws Exception{
		super.start(context);
		IPrologInterfaceService prologInterfaceService = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService();
		prologInterfaceService.registerPDTReloadExecutor(new ConsoleReloadExecutor());
	}

	private PrologConsoleService consoleService;

	private ErrorMessageProvider errorMessageProvider;

	public PrologConsoleService getPrologConsoleService() {
		if (consoleService == null) {
			consoleService = new DefaultPrologConsoleService();
		}
		return consoleService;
	}

	/**
	 * look up a preference value.
	 * <p>
	 * will return user settings if available or default settings if not. If a
	 * system property with the given key is defined it will overrule any
	 * existing setting in the preference store. if the key is not defined, this
	 * method returns the given default..
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 */
	public String getPreferenceValue(String key, String defaultValue) {

		IPreferencesService service = Platform.getPreferencesService();
		String qualifier = getBundle().getSymbolicName();
		String value = service.getString(qualifier, key, defaultValue, null);
		return System.getProperty(key, value);
	}

	public void setPreferenceValue(String key, String value) {
		getPreferenceStore().setValue(key, value);
	}

	public ErrorMessageProvider getErrorMessageProvider() {
		if (errorMessageProvider == null) {
			errorMessageProvider = new DefaultErrorMessageProvider(this);
		}
		return errorMessageProvider;
	}

	@Override
	public void earlyStartup() {
	}

}


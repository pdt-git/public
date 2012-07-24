/* $LICENSE_MSG$(ld) */


package org.cs3.prolog.ui.util;

import org.eclipse.core.runtime.Plugin;


public class DefaultErrorMessageProvider implements ErrorMessageProvider{

	private String id;
	private Plugin plugin;

	public DefaultErrorMessageProvider(Plugin plugin) {
		this.id =plugin.getBundle().getSymbolicName();
		this.plugin=plugin;
	}

	@Override
	public String getErrorMessage(int errCode) {
		return "unknown error("+errCode+")";
	}

	@Override
	public String getContextMessage(int cxCode) {
		return "unknown error context("+cxCode+")";
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public Plugin getPlugin() {
		return plugin;
	}

}


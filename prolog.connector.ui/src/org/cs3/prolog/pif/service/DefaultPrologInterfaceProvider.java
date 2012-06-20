package org.cs3.prolog.pif.service;

import org.cs3.prolog.connector.DefaultSubscription;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.Subscription;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;

public class DefaultPrologInterfaceProvider implements PrologInterfaceProvider {
	
	private static final String DEFAULT_CONSOLE = "Default Console";
	
	@Override
	public int getPriority() {
		return 0;
	}
	
	@Override
	public PrologInterface getPrologInterface() {
		PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		Subscription subscription = registry.getSubscription(DEFAULT_CONSOLE);
		if (subscription == null) {
			subscription = new DefaultSubscription(DEFAULT_CONSOLE + "_indepent", DEFAULT_CONSOLE, "Independent prolog process", DEFAULT_CONSOLE + " (Prolog)");
			registry.addSubscription(subscription);
		}
		PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(subscription);
		return pif;
	}
	
}

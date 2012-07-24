/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.connector;

import java.util.EventObject;

import org.cs3.prolog.pif.PrologInterface;

public class PrologInterfaceRegistryEvent extends EventObject {

	private static final long serialVersionUID = 1L;

	public PrologInterface pif = null;

	public String key = null;

	public Subscription subscription = null;

	public PrologInterfaceRegistryEvent(Object source, 
			PrologInterface pif,
			String key, 
			Subscription subscription) {
		super(source);
		this.pif = pif;
		this.key = key;
		this.subscription = subscription;
	}

	public PrologInterfaceRegistryEvent(PrologInterfaceRegistry reg,
			Subscription subscription) {
		super(reg);
		this.key = subscription.getPifKey();
		this.subscription = subscription;
		this.pif = reg.getPrologInterface(key);
	}

	
	public PrologInterfaceRegistryEvent(PrologInterfaceRegistry reg,
			String key) {
		super(reg);
		this.key = key;
		this.pif = reg.getPrologInterface(key);
	}
}


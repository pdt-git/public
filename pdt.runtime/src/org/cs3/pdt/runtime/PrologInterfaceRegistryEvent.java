package org.cs3.pdt.runtime;

import java.util.EventObject;

import org.cs3.pl.prolog.PrologInterface;

public class PrologInterfaceRegistryEvent extends EventObject {
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

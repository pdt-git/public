package org.cs3.pdt;

import java.util.EventObject;
import java.util.Set;

public class PreferencesEvent extends EventObject {

	private Set keys;

	public PreferencesEvent(Object source, Set keys) {
		super(source);		
		this.keys=keys;
	}
	
	
	public Set getKeys(){
		return keys;
	}

}

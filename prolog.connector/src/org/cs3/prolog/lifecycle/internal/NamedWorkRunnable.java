package org.cs3.prolog.lifecycle.internal;

public abstract class NamedWorkRunnable implements WorkRunnable {
	private final String name;

	NamedWorkRunnable(String name){
		this.name=name;
	}
	public final String getName(){
		return name;
	}
}

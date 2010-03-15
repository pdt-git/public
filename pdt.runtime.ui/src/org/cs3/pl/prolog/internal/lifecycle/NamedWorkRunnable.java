package org.cs3.pl.prolog.internal.lifecycle;

public abstract class NamedWorkRunnable implements WorkRunnable {
	private final String name;

	NamedWorkRunnable(String name){
		this.name=name;
	}
	public final String getName(){
		return name;
	}
}

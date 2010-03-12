package org.cs3.pl.prolog.internal.lifecycle;

public class DownState extends AbstractState {
	public DownState(LifeCycle cx){
		super(cx);
	}

public boolean isDown() {

	return true;
}
	
	public State start() {		
		return new InitState(context);
	}
	
	
	public State error(Throwable e) {	
		return this; //ignore errors while down.
	}
}

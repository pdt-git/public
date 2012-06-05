package org.cs3.pl.prolog.internal.lifecycle;

public class DownState extends AbstractState {
	public DownState(LifeCycle cx){
		super(cx);
	}

@Override
public boolean isDown() {

	return true;
}
	
	@Override
	public State start() {		
		return new InitState(context);
	}
	
	
	@Override
	public State error(Throwable e) {	
		return this; //ignore errors while down.
	}
}

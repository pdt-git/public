package org.cs3.pl.prolog.internal.lifecycle;

import org.cs3.pl.prolog.PrologInterfaceException;

public class Error2State extends AbstractState{

	private PrologInterfaceException error;


	protected Error2State(LifeCycle context,PrologInterfaceException error) {
		super(context);
		this.error=error;
	}
	
	
	
	public State error(Throwable e) {
		return this; // ignore further errors.
	}
	
	
	public PrologInterfaceException getError() {	
		return error;
	}
	
	
	public State reset() {
	
		return new DownState(context);
	}
}

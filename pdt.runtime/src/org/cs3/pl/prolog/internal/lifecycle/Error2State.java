package org.cs3.pl.prolog.internal.lifecycle;

import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterfaceException;

public class Error2State extends AbstractState{

	private PrologInterfaceException error;


	protected Error2State(LifeCycle context,PrologInterfaceException error) {
		super(context);
		this.error=error;
	}
	
	
	@Override
	public State error(Throwable e) {
		return this; // ignore further errors.
	}
	
	@Override
	public PrologInterfaceException getError() {	
		return error;
	}
	
	@Override
	public State reset() {
	
		return new DownState(context);
	}
}

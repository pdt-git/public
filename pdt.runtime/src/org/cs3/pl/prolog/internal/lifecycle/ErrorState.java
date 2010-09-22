package org.cs3.pl.prolog.internal.lifecycle;

import org.cs3.pl.prolog.PrologInterfaceException;

public class ErrorState extends AbstractState {

	private PrologInterfaceException error;
	private boolean shouldReset=false;

	public ErrorState(LifeCycle context, PrologInterfaceException e) {
		super(context);
		this.error = e;
	}

	
	@Override
	public PrologInterfaceException getError() {
		return error;
	}

	
	
	@Override
	public void enter() {		
		for (LifeCycleHookWrapper w : context.getHooks().values()) {
			w.onError();
		}
		context.enqueueWork(new NamedWorkRunnable("shutdown") {
			
			@Override
			public void run() throws PrologInterfaceException {
				try {
					context.disposeSessions();
					context.stopServer();
					context.workDone();
				} catch (Throwable e) {
					throw new PrologInterfaceException(e);
					
				}

			}
		});
	}

	
	@Override
	public State reset() {
		shouldReset=true;
		return this;
	}

	
	@Override
	public State workDone() {
		if(shouldReset){
			return new DownState(context);
		}
		return new Error2State(context,error);
	}

	
	@Override
	public State error(Throwable e) {
		return this; // ignore further errors.
	}

}

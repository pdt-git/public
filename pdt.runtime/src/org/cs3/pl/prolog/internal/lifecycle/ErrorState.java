package org.cs3.pl.prolog.internal.lifecycle;

import java.util.concurrent.BlockingQueue;

import org.cs3.pl.prolog.PrologInterfaceException;

public class ErrorState extends AbstractState {

	private PrologInterfaceException error;
	private boolean shouldReset=false;

	public ErrorState(LifeCycle context, PrologInterfaceException e) {
		super(context);
		this.error = e;
	}

	
	public PrologInterfaceException getError() {
		return error;
	}

	
	
	public void enter() {		
		for (LifeCycleHookWrapper w : context.getHooks().values()) {
			w.onError();
		}
		context.enqueueWork(new NamedWorkRunnable("shutdown") {
			
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

	
	public State reset() {
		shouldReset=true;
		return this;
	}

	
	public State workDone() {
		if(shouldReset){
			return new DownState(context);
		}
		return new Error2State(context,error);
	}

	
	public State error(Throwable e) {
		return this; // ignore further errors.
	}

}

package org.cs3.pl.prolog.internal.lifecycle;

import java.util.HashSet;

import org.cs3.pl.prolog.PrologInterfaceException;

public class ShutdownState extends AbstractState {

	public ShutdownState(LifeCycle context) {
		super(context);
	
	}

	
	
	public void enter() {
		
		HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
		context.getPrologInterface();
		context.clearWorkQueue(); //there may be afterINit hooks left. dump them.
		
		for (LifeCycleHookWrapper w : context.getHooks().values()) {			
			w.beforeShutdown(done);
		}
		
		context.enqueueWork(new NamedWorkRunnable("shutdown_server") {
			
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

	

	
	public State workDone() {	
		return new DownState(context); //reset when hooks are through.
	}

	

}

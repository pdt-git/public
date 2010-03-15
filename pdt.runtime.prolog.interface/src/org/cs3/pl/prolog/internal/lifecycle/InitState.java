package org.cs3.pl.prolog.internal.lifecycle;

import java.util.HashMap;
import java.util.HashSet;

import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterfaceException;

public class InitState extends AbstractState {

	protected InitState(LifeCycle context) {
		super(context);
	}

	public void enter() {
		HashSet<LifeCycleHookWrapper> done = new HashSet<LifeCycleHookWrapper>();
		HashMap<String, LifeCycleHookWrapper> hooks = context.getHooks();
		
		context.enqueueWork(new NamedWorkRunnable("startServer"){
			public void run() throws PrologInterfaceException {
				try {
					context.startServer();
				} catch (Throwable e) {
					throw new PrologInterfaceException(e);
				}
			}	
		});

		for (LifeCycleHookWrapper h : hooks.values()) {
			h.onInit(done);
		}

		context.enqueueWork(new NamedWorkRunnable("workDoneInit") {	
			public void run() throws PrologInterfaceException {
				context.workDone();
			}
		});
	}

	
	public State workDone() {
		return new AfterInitState(context);
	}

	
	public State addLifeCycleHook(final LifeCycleHook hook, String id,
			String[] dependencies) {
		if (isNewHook(hook,id)) {
			context.enqueueWork(new NamedWorkRunnable("lateInit on"+id) {
				public void run() throws PrologInterfaceException {
					hook.lateInit(context.getPrologInterface());
				}
			});
		}
		return super.addLifeCycleHook(hook, id, dependencies);
	}

	private boolean isNewHook(LifeCycleHook hook,String id) {
		LifeCycleHookWrapper w= context.getHooks().get(id);
		return w==null || ! w.hooks.contains(hook);		
	}
}

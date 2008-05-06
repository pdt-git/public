package org.cs3.pl.prolog.internal.lifecycle;

import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook3;
import org.cs3.pl.prolog.PrologInterfaceException;

public class UpState extends AbstractState {

	protected UpState(LifeCycle context) {
		super(context);
	}
	
	public boolean isUp() {
		return true;
	}
	
	public State addLifeCycleHook(final LifeCycleHook hook, String id,
			String[] dependencies) {
		if (hook instanceof LifeCycleHook3 && isNewHook(hook,id)) {
			context.enqueueWork(new NamedWorkRunnable("lateInit_on_"+id) {
				public void run() throws PrologInterfaceException {
					((LifeCycleHook3)hook).lateInit(context.getPrologInterface());
				}
			});
		}
		return super.addLifeCycleHook(hook, id, dependencies);
	}

	private boolean isNewHook(LifeCycleHook hook,String id) {
		LifeCycleHookWrapper w= context.getHooks().get(id);
		return w==null || ! w.hooks.contains(hook);		
	}
	
	
	public State stop() {	
		return new ShutdownState(context);
	}

}

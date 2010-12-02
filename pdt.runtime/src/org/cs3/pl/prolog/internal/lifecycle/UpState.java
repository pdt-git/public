package org.cs3.pl.prolog.internal.lifecycle;

import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterfaceException;

public class UpState extends AbstractState {

	protected UpState(LifeCycle context) {
		super(context);
	}
	
	@Override
	public boolean isUp() {
		return true;
	}
	
	@Override
	public State addLifeCycleHook(final LifeCycleHook hook, String id,
			String[] dependencies) {
		if (isNewHook(hook,id)) {
			context.enqueueWork(new NamedWorkRunnable("lateInit_on_"+id) {
				@Override
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
	
	
	@Override
	public State stop() {	
		return new ShutdownState(context);
	}

}

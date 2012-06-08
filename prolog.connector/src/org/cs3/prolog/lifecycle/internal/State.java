/**
 * 
 */
package org.cs3.prolog.lifecycle.internal;

import org.cs3.prolog.lifecycle.LifeCycleHook;
import org.cs3.prolog.pif.PrologInterfaceException;

public interface State{
	public void enter();
	public PrologInterfaceException getError();
	public boolean isUp();
	public boolean isDown();
	
	public State start();
	public State stop();
	public State error(Throwable e);
	public State workDone();
	public State addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies);
	public State removeLifeCycleHook(final String hookId);
	public State removeLifeCycleHook(final LifeCycleHook hook,
			final String hookId);
	public State reset();
	
}
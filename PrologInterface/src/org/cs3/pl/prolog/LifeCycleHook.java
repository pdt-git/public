package org.cs3.pl.prolog;

public interface LifeCycleHook{
	abstract void onInit(PrologSession initSession);
	abstract void afterInit();
	abstract void beforeShutdown(PrologSession session);	
}

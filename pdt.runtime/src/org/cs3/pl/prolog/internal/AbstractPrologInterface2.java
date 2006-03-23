package org.cs3.pl.prolog.internal;

import java.lang.ref.WeakReference;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologSession;

public abstract class AbstractPrologInterface2 extends AbstractPrologInterface implements PrologInterface2{
	 public abstract AsyncPrologSession getAsyncSession_impl() throws Throwable;

	    public AsyncPrologSession getAsyncSession() {
	        synchronized (stateLock) {
	            if(START_UP==getState()){
	                if(theThreadWhoDidIt==Thread.currentThread()){
	                    Debug.error("getSession() called from init thread. Please read the api docs for LifeCycleHook.onInit(PrologSession).");
	                    throw new IllegalThreadStateException("You cannot call getSession() from the init thread during pif startup.");
	                }
	                waitUntilUp();
	            }
	            if(UP!=getState()){
	                throw new IllegalStateException("Cannot create session. Not in UP state.");
	            }
	            try {
	                return getAsyncSession_internal();
	            } catch (Throwable t) {
	                Debug.report(t);
	                throw new RuntimeException(t);
	            }
	        }
	    }
	    
	    
	    private AsyncPrologSession getAsyncSession_internal() throws Throwable {
	        synchronized (stateLock) {
	                            AsyncPrologSession s = getAsyncSession_impl();
	                sessions.add(new WeakReference(s));
	                return s;
	            
	        }
	    }
}

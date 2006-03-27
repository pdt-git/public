package org.cs3.pl.prolog.internal;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;


public class LifeCycleHookWrapper implements LifeCycleHook{
	
	String id;
	/**things i depend on*/
	public Vector post= new Vector();
	
	/**things that depend on me*/
	public Vector pre= new Vector();
	
	public LifeCycleHook hook;
	public boolean flipflop= false;
	public LifeCycleHookWrapper(LifeCycleHook hook,String id ) {
		this.hook = hook;
		this.id=id;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
	 */
	public void onInit(PrologInterface pif,PrologSession initSession) {
		flipflop=!flipflop;
		for (Iterator it = post.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm= (LifeCycleHookWrapper) it.next();
			if(elm.flipflop!=this.flipflop){
				elm.onInit(pif,initSession);
			}
		}		
		if(hook!=null){
		    Debug.info("enter onInit() on hook "+id);
		    try{
		    	hook.onInit(pif,initSession);
		    }
		    catch(Throwable t){
	    			Debug.error("onInit() failed for hook: "+id+", exception trace follows:");
	    			Debug.report(t);
		    }
			Debug.info("exit onInit() on hook "+id);
		}
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit(PrologInterface pif) {
		flipflop=!flipflop;
		for (Iterator it = post.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm= (LifeCycleHookWrapper) it.next();
			if(elm.flipflop!=this.flipflop){
				elm.afterInit(pif);
			}
		}		
		if(hook!=null){
		    Debug.info("enter afterInit() on hook "+id);
		    try{
		    	hook.afterInit(pif);
		    }
		    catch(Throwable t){
		    		Debug.error("afterInit() failed for hook: "+id+", exception trace follows:");
		    		Debug.report(t);
		    }
			Debug.info("exit afterInit() on hook "+id);
		}	
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologInterface pif,PrologSession session) {
		flipflop=!flipflop;
		for (Iterator it = pre.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm= (LifeCycleHookWrapper) it.next();
			if(elm.flipflop!=this.flipflop){
				elm.beforeShutdown(pif,session);
			}
		}		
		if(hook!=null){
		    Debug.info("enter beforeShutdown() on hook "+id);
		    try{
		    		hook.beforeShutdown(pif,session);
		    }
		    catch(Throwable t){
	    			Debug.error("beforeShutdown() failed for hook: "+id+", exception trace follows:");
	    			Debug.report(t);
		    }
			Debug.info("exit beforeShutdown() on hook "+id);
		}
	}

	

}

package org.cs3.pl.prolog;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;


public class LifeCycleHookWrapper implements LifeCycleHook{
	
	String id;
	/**things i depend on*/
	Vector post= new Vector();
	
	/**things that depend on me*/
	Vector pre= new Vector();
	
	LifeCycleHook hook;
	boolean flipflop= false;
	public LifeCycleHookWrapper(LifeCycleHook hook,String id ) {
		this.hook = hook;
		this.id=id;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
	 */
	public void onInit(PrologSession initSession) {
		flipflop=!flipflop;
		for (Iterator it = post.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm= (LifeCycleHookWrapper) it.next();
			if(elm.flipflop!=this.flipflop){
				elm.onInit(initSession);
			}
		}		
		if(hook!=null){
		    Debug.info("excecuting onInit() on hook "+id);
			hook.onInit(initSession);
		}
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit() {
		flipflop=!flipflop;
		for (Iterator it = post.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm= (LifeCycleHookWrapper) it.next();
			if(elm.flipflop!=this.flipflop){
				elm.afterInit();
			}
		}		
		if(hook!=null){
		    Debug.info("excecuting afterInit() on hook "+id);
			hook.afterInit();
		}	
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologSession session) {
		flipflop=!flipflop;
		for (Iterator it = pre.iterator(); it.hasNext();) {
			LifeCycleHookWrapper elm= (LifeCycleHookWrapper) it.next();
			if(elm.flipflop!=this.flipflop){
				elm.beforeShutdown(session);
			}
		}		
		if(hook!=null){
		    Debug.info("excecuting beforeShutdown() on hook "+id);
			hook.beforeShutdown(session);
		}
	}

	

}

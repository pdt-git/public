/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pl.prolog.internal;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;


public class LifeCycleHookWrapper implements LifeCycleHook2{
	
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

	public void onError(PrologInterface pif) {
		//ignore dependencies, just do it.
		if(hook!=null&&hook instanceof LifeCycleHook2){
		    Debug.info("enter onError() on hook "+id);
		    try{
		    		((LifeCycleHook2)hook).onError(pif);
		    }
		    catch(Throwable t){
	    			Debug.error("onError() failed for hook: "+id+", exception trace follows:");
	    			Debug.report(t);
		    }
			Debug.info("exit onError() on hook "+id);
		}
	}

	

}

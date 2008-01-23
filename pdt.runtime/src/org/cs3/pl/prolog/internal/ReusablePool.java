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

/*
 */
package org.cs3.pl.prolog.internal;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pl.common.Debug;

/**
 * Creation of new client sessions seems to be rather costly due to the involved
 * rpc overhead. So we try to pool unused sessions for later use.
 */
public class ReusablePool {
	HashMap pool = new HashMap();

	int maxPoolSize = 5;

	int poolSize = 0;


	public void recycle(Reusable s) {
	    s.recylce();
		addInstance(s);
	}

	protected void addInstance(Reusable s) {
		synchronized (pool) {
            Debug.debug("poolSize="+poolSize+", maxPoolSize="+maxPoolSize);
			if (poolSize >= maxPoolSize) {
				Debug.debug("maximum pool size exeeded. instance destroyed.");                
				s.destroy();				
				return;
			}			
			Class clazz = s.getClass();

			LinkedList l = (LinkedList) pool.get(clazz);
			if (l == null) {
				l = new LinkedList();
				pool.put(clazz, l);
			}
			l.addLast(s);
			
			poolSize++;
			Debug.debug("new instance added to the pool. poolSize now: "+poolSize);
		}
	}

	public Reusable findInstance(Class clazz) {
		Reusable r = null;
		synchronized (pool) {
			LinkedList l = (LinkedList) pool.get(clazz);
			if (l == null) {
				Debug.debug("no reusable instance in pool");
				return null;
			}
			if (l.isEmpty()) {
				Debug.debug("no reusable instance in pool");
				return null;
			}
			poolSize--;
			r = (Reusable) l.removeFirst();

		}
		r.reuse();
		Debug.debug("instance taken from pool and reanimated. poolSize now: "+poolSize);
		return r;
	}

	public int getMaxTotalSize() {
		synchronized (pool) {
			return maxPoolSize;
		}
	}

	public void setMaxTotalSize(int size) {
		synchronized (pool) {
			this.maxPoolSize = size;
		}
	}
	// TRHO: fixed PDT-262
	public void clear(){
		synchronized (pool) {
		Collection collection = pool.values();
			for (Iterator it = collection.iterator(); it.hasNext();) {
				List list = (List) it.next();
				for (Iterator it2 = list.iterator(); it2.hasNext();) {
					Reusable s = (Reusable) it2.next();
					s.destroy();
					//it2.remove();
				}
				list.clear();
				//it.remove();
			}
			pool.clear();
			poolSize=0;
		}
	}

}
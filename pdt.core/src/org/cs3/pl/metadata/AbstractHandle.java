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


package org.cs3.pl.metadata;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.runtime.PLUtil;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession2;

public abstract  class AbstractHandle implements Handle, PrologInterfaceListener {
	IPrologProject project = null;
	HashMap cache = new HashMap();
	private Vector listeners=new Vector();
	
	

	public void lookup() throws InvalidHandleExcpetion, PrologInterfaceException {
		if(project==null){
			return;
		}
		String query="pdt_lookup("+constructHandleTerm()+")"; 
		PrologSession2 session = (PrologSession2) project.getMetadataPrologInterface().getSession();		
		try{
			session.setPreferenceValue("socketsession.canonical", "true");
			List list = session.queryAll(query);
			if(list.size()>1){
				throw new InvalidHandleExcpetion("Handle is not unique! The following query produced more than one result: "+query);
			}
			if(list.size()==0){
				throw new InvalidHandleExcpetion("Handle lookup failed: "+query);
			}
			Map m = (Map) list.get(0);
			CTerm term = (CTerm) m.get("Cache");
			cache=(HashMap) PLUtil.listAsMap(term);
			fireCacheUpdated();
			
		}finally{
			if(session!=null){
				session.dispose();
			}
		}

	}

	

	public void setCachedProperty(String name, CTerm value) {
		cache.put(name,value);
		fireCacheUpdated();
	}

	public CTerm getCachedProperty(String name) {
		return (CTerm) cache.get(name);
	}

	public void addEntityListener(EntityListener l) throws PrologInterfaceException {
		
		synchronized (listeners) {
			
			if(!listeners.contains(l)){
				if(listeners.isEmpty()&&project!=null){
					
					PrologEventDispatcher dispatch = project.getMetaDataEventDispatcher();
					dispatch.addPrologInterfaceListener(constructHandleTerm(),this);
				}
				listeners.add(l);
			}
		}
		

	}

	public void removeEntityListener(EntityListener l) throws PrologInterfaceException {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
				if(listeners.isEmpty()&&project!=null){
					PrologEventDispatcher dispatch = project.getMetaDataEventDispatcher();
					dispatch.removePrologInterfaceListener(constructHandleTerm(),this);
				}
			}
		}
		

	}

	public void update(PrologInterfaceEvent e) {
		String event =e.getEvent();
		if("update".equals(event)){
			fireEntityUpdated();
		}
		else if("forget".equals(event)){
			fireEntityDeleted();
		}
	}

	

	


	public AbstractHandle(IPrologProject project) {
		
		this.project = project;
	}

	/**
	 * @deprecated this constructor only exists to support legazy code.
	 *
	 */
	public AbstractHandle() {
		
		
	}
	
	private void fireCacheUpdated() {
		EntityEvent e = new EntityEvent(this);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			EntityListener l = (EntityListener) it.next();
			l.cacheUpdated(e);
		}
	}
	
	private void fireEntityDeleted() {
		EntityEvent e = new EntityEvent(this);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			EntityListener l = (EntityListener) it.next();
			l.entityDeleted(e);
		}
		
	}


	private void fireEntityUpdated() {
		EntityEvent e = new EntityEvent(this);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			EntityListener l = (EntityListener) it.next();
			l.entityUpdated(e);
		}
		
	}
	
	public boolean equals(Object obj) {
		if(!(obj instanceof Handle)){
			return false;
		}
		Handle other = (Handle) obj;
		return this.constructHandleTerm().equals(other.constructHandleTerm());
	}

	public int hashCode() {
		return constructHandleTerm().hashCode();
	}



	
}

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

package org.cs3.pdt.runtime;

import java.util.HashMap;
import java.util.Map;

import org.cs3.pl.prolog.PrologInterface;


/**
 * Simple data structure for storing information on a particular use of 
 * a PrologInterface instance.
 * 
 * Currently, only the pif's registry key and an informal description 
 * is stored.  
 * 
 * If the need should arise, we may add arbitrary other "tags". 
 *
 */
public class DefaultSubscription implements PersistableSubscription {
	
	private String descritpion;
	
	private String hostId;
	
	private String id;

	private String name;

	private boolean persistent;

	private String pifKey;
	
	/**
	 * clients should not use this constructor. Its only called
	 * via reflection when the subscription is restored.
	 */
	public DefaultSubscription(){
		
	};
	
	public DefaultSubscription(String id,
			   String pifID,  
			   String descritpion, 
			   String name){
		this(id,pifID,descritpion,name,null,false);
	}
	
	public DefaultSubscription(String id,
							   String pifID,  
							   String descritpion, 
							   String name,
							   String hostId,
							   boolean persistent) {
		this.setPifKey(pifID);
		this.setId(id);
		this.setDescritpion(descritpion);
		this.setName(name);
		this.setHostId(hostId);
		this.setPersistent(persistent);
		
	}



	

	/**
	 * the default implementation does nothing.
	 */
	public void configure(PrologInterface pif) {
		;		
	}


	/**
	 * the default implementation does nothing.
	 */	
	public void deconfigure(PrologInterface pif) {
		;
	}



	/* (non-Javadoc)
	 * @see org.cs3.pdt.runtime.Subscription#getDescritpion()
	 */
	public String getDescritpion() {
		return descritpion;
	}



	public String getHostId() {		
		return hostId;
	}



	public String getId() {
		
		return id;
	}



	public String getName() {
		return this.name;		
	}



	/* (non-Javadoc)
	 * @see org.cs3.pdt.runtime.Subscription#getKey()
	 */
	public String getPifKey() {
		return pifKey;
	}



	public boolean isPersistent() {
		return persistent;
	}



	public void restoreState(Map params) {
		setDescritpion((String) params.get("description"));
		setName((String) params.get("name"));
		setHostId((String) params.get("hostid"));
		setPifKey((String) params.get("pifkey"));
		setId((String) params.get("id"));
		setPersistent((String) params.get("persistent"));
		
	}



	private void setPersistent(String string) {
		setPersistent(Boolean.valueOf(string).booleanValue());		
	}

	public Map saveState() {
		Map m = new HashMap();
		m.put("description",getDescritpion());
		m.put("name",getName());
		m.put("hostid",getHostId());
		m.put("pifkey",getPifKey());
		m.put("id",getId());
		m.put("persistent",String.valueOf(isPersistent()));
		return m;
	}



	protected void setDescritpion(String descritpion) {
		this.descritpion = descritpion;
	}



	protected void setHostId(String hostId) {
		this.hostId = hostId;
	}



	protected void setId(String id) {
		this.id=id;
		
	}



	protected void setName(String name) {
		this.name = name;
	}



	protected void setPersistent(boolean persistent) {
		this.persistent = persistent;
	}



	protected void setPifKey(String key) {
		this.pifKey = key;
	}
	
}

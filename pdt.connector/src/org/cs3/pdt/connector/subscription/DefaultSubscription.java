/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.connector.subscription;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.common.Util;
import org.cs3.prolog.connector.process.PrologInterface;


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
	private String[] tags = new String[0];
	
	/**
	 * clients should not use this constructor. Its only called
	 * via reflection when the subscription is restored.
	 */
	public DefaultSubscription(){
		
	};

	/**
	 * Only for non-persistent subscriptions
	 * @param id
	 * @param pifID
	 * @param descritpion
	 * @param name
	 */
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

	@Override
	public void configure(PrologInterface pif) {
		;		
	}

	@Override
	public void deconfigure(PrologInterface pif){
		;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pdt.runtime.Subscription#getDescritpion()
	 */
	@Override
	public String getDescritpion() {
		return descritpion;
	}

	@Override
	public String getHostId() {		
		return hostId;
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getName() {
		return this.name;		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pdt.runtime.Subscription#getKey()
	 */
	@Override
	public String getPifKey() {
		return pifKey;
	}

	@Override
	final public boolean isPersistent() {
		return persistent;
	}

	@Override
	public void restoreState(Map<String, String> params) {
		setDescritpion(params.get("description"));
		setName(params.get("name"));
		setHostId(params.get("hostid"));
		setPifKey(params.get("pifkey"));
		setId(params.get("id"));
		setPersistent(params.get("persistent"));
		if(params.get("tags")!= null) {
			setTags(Util.split(params.get("tags"), ","));
		}
	}

	private void setPersistent(String string) {
		setPersistent(Boolean.valueOf(string).booleanValue());		
	}

	@Override
	public Map<String, String> saveState() {
		Map<String, String> m = new HashMap<String, String>();
		m.put("description",getDescritpion());
		m.put("name",getName());
		m.put("hostid",getHostId());
		m.put("pifkey",getPifKey());
		m.put("id",getId());
		m.put("persistent",String.valueOf(isPersistent()));
		m.put("tags",Util.splice(tags, ","));
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

	@Override
	public boolean isVisible() {
		return true;
	}

	@Override
	public String[] getTags() {
		return tags;
	}

	public void setTags(String[] tags) {
		this.tags = tags;
	}
	
	@Override
	public Object getData() {		
		return null;
	}

	@Override
	public List<String> getBootstrapConstributionKeys() {
		return new ArrayList<String>();
	}
}



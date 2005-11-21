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

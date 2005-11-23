package org.cs3.pdt.runtime;

import java.util.List;
import java.util.Set;

import org.cs3.pl.prolog.PrologInterface;

/**
 * manages a set of prolog interface instances. <br>
 * This class manages a relation consisting of tupels that are of the form
 * (PrologInterface pif,Object client,String use)
 * 
 * @author lukas
 */
public interface PrologInterfaceRegistry {


	/**
	 * @return all keys to which PrologInterfaces are registered.
	 */
	public Set getRegisteredKeys();

	/**
	 * @return all keys that are known to the registry, including keys
	 * for which no pif is registered.
	 */
	public Set getAllKeys();
	
	/**
	 * @return the IDs of all subscriptions registered with the 
	 * registry..
	 */
	public Set getAllSubscriptionIDs();
	
	/**
	 * return all subscriptions to a given registered pif key
	 * @return null if no such pif, empty list if no subscriptions
	 */	
	public List getSubscriptionsForPif(String key);

	/**
	 * A short catchy name associated with a given pif.
	 * Maybe null if no such pif, or no name. 
	 * @deprecated the method may be removed, due to its unclear semantics see PDT-108
	 */
	public String getName(String key);
	
	/**
	 * Set an optional name for a given pif identifier.
	 * @deprecated the method may be removed, due to its unclear semantics see PDT-108
	 */
	public void setName(String key, String name);

	
	/**
	 * retrieve the registry key of a registered PrologInterface.	 * 
	 */
	public String getKey(PrologInterface prologInterface);

	/**
	 * retrieve the PrologInterface instance registered for the given key.
	 */
	public PrologInterface getPrologInterface(String key);

	
	public void addPrologInterfaceRegistryListener(PrologInterfaceRegistryListener l);
	public void removePrologInterfaceRegistryListener(PrologInterfaceRegistryListener l);
	
	
	public void addPrologInterface(String key, PrologInterface pif);
	public void removePrologInterface(String key);
	
	public void addSubscription(Subscription s);
	public void removeSubscription(Subscription s);

	public Subscription getSubscription(String key);
}

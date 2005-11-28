package org.cs3.pdt.runtime;

import java.util.Set;

import org.cs3.pl.prolog.PrologInterface;

/**
 * Central registry for managing PrologInterface instances and Subscriptions.
 * 
 * The registry keeps track of every PrologInterface created by the pdt.runtime
 * plugin. In addition, clients can register Subscriptions to particular
 * PrologInterface instances, thereby publically announcing what they intend to use them for.
 * 
 * This registry is ment to be a kind of "forum" for clients that need to share one and the same PrologInterface instance.
 * It is also intended to provide a model for ui components that need to provide the user with a choice of available Prolog runtimes.
 * 
 * Note that adding a Subscription means subscribing to a particular PrologInterface _KEY_ rather than to the instance itself.
 * Among other things this allows clients to express ther instance in a particular PrologInterface before it actually exists.
 * This is important since PrologInterface instances typically get created in a lazy fashion, whereas the ui should be able 
 * to reflect subscriptions much earlier to help the user understand her environment better. 
 * 
 * @author lukas
 */
public interface PrologInterfaceRegistry {

	/**
	 * @return all keys to which PrologInterfaces are registered.
	 */
	public Set getRegisteredKeys();

	/**
	 * @return all keys that are known to the registry, including keys for which
	 *         no pif is registered.
	 */
	public Set getAllKeys();

	/**
	 * @return the IDs of all subscriptions registered with the registry..
	 */
	public Set getAllSubscriptionIDs();

	/**
	 * return all subscriptions to a given pif key
	 * 
	 * @return null if no such pif, empty list if no subscriptions
	 */
	public Set getSubscriptionsForPif(String key);

	/**
	 * return the keys of all subscriptions to a given pif key
	 * 
	 * @return null if no such pif, empty list if no subscriptions
	 */
	public Set getSubscriptionKeysForPif(String key);

	/**
	 * A short catchy name associated with a given pif. Maybe null if no such
	 * pif, or no name.
	 * 
	 * @deprecated the method will be removed, due to its unclear semantics see
	 *             PDT-108
	 */
	public String getName(String key);

	/**
	 * Set an optional name for a given pif identifier.
	 * 
	 * @deprecated the method will be removed, due to its unclear semantics see
	 *             PDT-108
	 */
	public void setName(String key, String name);

	/**
	 * retrieve the registry key of a registered PrologInterface. *
	 */
	public String getKey(PrologInterface prologInterface);

	/**
	 * retrieve the PrologInterface instance registered for the given key.
	 */
	public PrologInterface getPrologInterface(String key);

	/**
	 * add a listener to this registry.
	 * 
	 * Listeners get notified whenever a PrologInterface instance or
	 * Subscription is registered or unregistered.
	 * 
	 * @param l
	 */
	public void addPrologInterfaceRegistryListener(
			PrologInterfaceRegistryListener l);

	/**
	 * remove a listener from this registry.
	 * 
	 * @param l
	 */
	public void removePrologInterfaceRegistryListener(
			PrologInterfaceRegistryListener l);

	/**
	 * Register a PrologInterface with this registry.
	 * 
	 * This method will cause a call to the method configure() on any waiting
	 * subscriptions that are already registered for the given pifkey.
	 * 
	 * @param key
	 * @param pif
	 */
	public void addPrologInterface(String key, PrologInterface pif);

	/**
	 * Remove a PrologInterface from this registry.
	 * 
	 * Removes the PrologInterface with the given key. If no PrologInterface was
	 * registered for that key, this method has no effect. Subscriptions will
	 * NOT be removed.
	 * 
	 * This method will cause a call to the method deconfigure() on any
	 * subscription registered for the given pif key.
	 * 
	 * @param key
	 * 
	 */
	public void removePrologInterface(String key);

	/**
	 * Add a subscription to the registry.
	 * 
	 * If there is already a PrologInterface instance registered for the
	 * subscriptions pifKey, this method will cause a call to the method
	 * configure() on the argument Subscription instance.
	 * 
	 * If the argument Subscription is an instance of PersistableSubscription,
	 * the registry will take the neccesary steps to save the subscription on
	 * workbench shutdown and restore it on the next startup.
	 * 
	 * @param s
	 */
	public void addSubscription(Subscription s);

	/**
	 * Remove a subscription from the registry.
	 * 
	 * If there is currently a PrologInterface instance registered for the
	 * subscriptions pifKey, this method will cause a call to the method
	 * deconfigure() on the argument Subscription instance.
	 * 
	 * @param s
	 */
	public void removeSubscription(Subscription s);

	/**
	 * Remove a subscription from the registry.
	 * 
	 * Removes the subscription with the given subscription id.
	 * 
	 * @see removeSubscription(Subscription)
	 * @param id
	 */
	public void removeSubscription(String id);

	/**
	 * Find the Subscription for a given subscription id;
	 * 
	 * @param id
	 * @return the Subscription or null if none was registered with this id.
	 */
	public Subscription getSubscription(String id);
}

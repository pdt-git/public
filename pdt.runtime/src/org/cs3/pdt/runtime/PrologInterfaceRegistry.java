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

import java.util.Set;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;

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
	 * If another PrologInterface is already registered with this key,
	 * it is removed first.
	 * 
	 * If the same PrologInterace is already registered with this key,
	 * this method has no effect.
	 * 
	 * This method will cause a call to the method configure() on any waiting
	 * subscriptions that are already registered for the given pifkey.
	 * 
	 * @param key
	 * @param pif
	 * @throws PrologInterfaceException 
	 */
	public void addPrologInterface(String key, PrologInterface pif) ;

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
	public void removePrologInterface(String key) throws PrologInterfaceException;

	/**
	 * Add a subscription to the registry.
	 * 
	 * If there is already a Subscription with the same key, it will be removed
	 * first.
	 * 
	 * If the same subscription is already registered, this method has no effect.
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
	 * @throws PrologInterfaceException 
	 */
	public void addSubscription(Subscription s) ;

	/**
	 * Remove a subscription from the registry.
	 * 
	 * If there is currently a PrologInterface instance registered for the
	 * subscriptions pifKey, this method will cause a call to the method
	 * deconfigure() on the argument Subscription instance.
	 * 
	 * @param s
	 * @throws PrologInterfaceException 
	 */
	public void removeSubscription(Subscription s);

	/**
	 * Remove a subscription from the registry.
	 * 
	 * Removes the subscription with the given subscription id.
	 * 
	 * @see removeSubscription(Subscription)
	 * @param id
	 * @throws PrologInterfaceException 
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

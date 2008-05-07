package org.cs3.pdt.runtime.internal;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;

import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologInterfaceRegistryEvent;
import org.cs3.pdt.runtime.PrologInterfaceRegistryListener;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;

public class DefaultPrologInterfaceRegistry implements PrologInterfaceRegistry {

	private HashMap<String, HashSet<LifeCycleHookDescriptor>> hookDescriptors = new HashMap<String, HashSet<LifeCycleHookDescriptor>>();
	private HashMap<String, PrologInterface> pifs = new HashMap<String, PrologInterface>();
	private HashMap<String, Subscription> subscriptions = new HashMap<String, Subscription>();
	private HashMap<String, HashSet<Subscription>> subscriptionLists = new HashMap<String, HashSet<Subscription>>();
	private Vector<PrologInterfaceRegistryListener> listeners = new Vector<PrologInterfaceRegistryListener>();
	private HashMap<PrologInterface, String> pifKeys = new HashMap<PrologInterface, String>();

	public void addPrologInterfaceRegistryListener(
			PrologInterfaceRegistryListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	public void removePrologInterfaceRegistryListener(
			PrologInterfaceRegistryListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}

	}

	public void firePrologInterfaceAdded(String key) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				key);
		Vector<PrologInterfaceRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologInterfaceRegistryListener>) listeners.clone();
		}
		for (PrologInterfaceRegistryListener l : clone) {				
			l.prologInterfaceAdded(e);
		}
	}

	public void firePrologInterfaceRemoved(String key) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				key);
		Vector<PrologInterfaceRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologInterfaceRegistryListener>) listeners.clone();
		}
		for (PrologInterfaceRegistryListener l : clone) {
			l.prologInterfaceRemoved(e);
		}
	}

	public void fireSubscriptionAdded(Subscription s) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				s);
		Vector<PrologInterfaceRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologInterfaceRegistryListener>) listeners.clone();
		}
		for (PrologInterfaceRegistryListener l : clone) {
			l.subscriptionAdded(e);
		}
	}

	public void fireSubscriptionRemoved(Subscription s) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				s);
		Vector<PrologInterfaceRegistryListener> clone = null;
		synchronized (listeners) {
			clone = (Vector<PrologInterfaceRegistryListener>) listeners.clone();
		}
		for (PrologInterfaceRegistryListener l : clone) {
			l.subscriptionRemoved(e);
		}
	}

	public Set<String> getRegisteredKeys() {

		return pifs.keySet();
	}

	public String getKey(PrologInterface prologInterface) {
		return pifKeys.get(prologInterface);
	}

	public PrologInterface getPrologInterface(String key) {
		return pifs.get(key);
	}

	public Subscription getSubscription(String key) {
		return subscriptions.get(key);

	}

	public Set<String> getAllKeys() {
		Set<String> s = new HashSet<String>(getRegisteredKeys());
		s.addAll(subscriptionLists.keySet());
		return s;
	}

	public Set<String> getAllSubscriptionIDs() {
		return new HashSet<String>(subscriptions.keySet());
	}

	public Set<Subscription> getAllSubscriptions() {
		return new HashSet<Subscription>(subscriptions.values());
	}
	
	public Set<Subscription> getSubscriptionsForPif(String key) {
		Collection coll = subscriptionLists.get(key);
		HashSet<Subscription> subscripitions = new HashSet<Subscription>();
		if(coll != null){
			subscripitions.addAll(coll);
		}
		return subscripitions;
	}

	public void addPrologInterface(String key, PrologInterface pif) {
		Object old = pifs.get(key);
		if (old == pif) {
			return;
		}
		if (old != null) {
			removePrologInterface(key);
		}
		pifs.put(key, pif);
		pifKeys.put(pif, key);
		Set<Subscription> l = getSubscriptionsForPif(key);
		for (Subscription s: l) {
			
		
			Set<LifeCycleHookDescriptor> descriptors = getHookDescriptorsForTags(s
					.getTags());
			for (LifeCycleHookDescriptor desc : descriptors) {
				pif.addLifeCycleHook(desc.createHook(s.getData()), desc
						.getHookId(), desc.getHookDependencies());
			}
			s.configure(pif);

		}
		firePrologInterfaceAdded(key);
	}

	public void removePrologInterface(String key) {
		PrologInterface pif = pifs.get(key);
		if (pif == null) {
			return;
		}
		HashSet<Subscription> l =  subscriptionLists.get(key);
		if (l != null) {
			l =  (HashSet<Subscription>) l.clone();
			for (Subscription s : l) {
							
				s.deconfigure(pif);

				Set<LifeCycleHookDescriptor> descriptors = getHookDescriptorsForTags(s
						.getTags());
				for (LifeCycleHookDescriptor desc : descriptors) {
					((PrologInterface2) pif).removeLifeCycleHook(desc
							.createHook(s.getData()), desc.getHookId());
				}
			}
		}

		firePrologInterfaceRemoved(key);
		pifKeys.remove(pif);
		pifs.remove(key);

	}

	public void addSubscription(Subscription s) {
		// do not add anonymous subscriptions
		String sid = s.getId();
		if (sid == null) {
			return;
		}

		Object old = subscriptions.get(sid);
		if (old == s) {
			return;
		}
		if (old != null) {
			removeSubscription(sid);
		}
		HashSet<Subscription> l = subscriptionLists.get(s.getPifKey());
		if (l == null) {
			l = new HashSet<Subscription>();
			subscriptionLists.put(s.getPifKey(), l);
		}
		l.add(s);
		subscriptions.put(sid, s);

		if (this.pifs.containsKey(s.getPifKey())) {
			Set<LifeCycleHookDescriptor> descriptors = getHookDescriptorsForTags(s
					.getTags());
			for (LifeCycleHookDescriptor desc : descriptors) {
				((PrologInterface2) pifs.get(s.getPifKey()))
						.removeLifeCycleHook(desc.createHook(s.getData()), desc
								.getHookId());
			}
			s.configure(getPrologInterface(s.getPifKey()));
		}
		fireSubscriptionAdded(s);
	}

	public void removeSubscription(String id) {
		removeSubscription(getSubscription(id));
	}

	public void removeSubscription(Subscription s) {
		// do not remove anonymous subscriptions
		if (s.getId() == null) {
			return;
		}
		if (!subscriptions.containsKey(s.getId())) {
			return;
		}
		if (pifs.containsKey(s.getPifKey())) {
			s.deconfigure(getPrologInterface(s.getPifKey()));
			Set<LifeCycleHookDescriptor> descriptorsDiff = getHookDescriptorsForTags(s
					.getTags());
			Set<Subscription> otherSubscriptions = getSubscriptionsForPif(s
					.getPifKey());
			otherSubscriptions.remove(s);
			Set<LifeCycleHookDescriptor> descriptorsAfter = getHookDescriptorsForTags(getTagsForSubscriptions(otherSubscriptions));
			descriptorsDiff.removeAll(descriptorsAfter);
			for (LifeCycleHookDescriptor desc : descriptorsDiff) {

				((PrologInterface2) getPrologInterface(s.getPifKey()))
						.removeLifeCycleHook(desc.createHook(s.getData()), desc
								.getHookId());
			}
		}
		subscriptions.remove(s.getId());

		Set l = subscriptionLists.get(s.getPifKey());
		if (l == null) {
			return;
		}
		if (l.contains(s.getId())) {
			l.remove(s.getId());
			fireSubscriptionRemoved(s);

		}

	}

	
	public void addHookDescriptor(LifeCycleHookDescriptor descr) {

		HashSet<LifeCycleHookDescriptor> l = hookDescriptors.get(descr.getHookId());
		if (l == null) {
			l = new HashSet<LifeCycleHookDescriptor>();
			hookDescriptors.put(descr.getHookId(), l);
		}
		if (!l.contains(descr)) {
			l.add(descr);
			Set<Subscription> subs = getSubscriptionsForTags(descr.getTags());
			for (Subscription subscription : subs) {
				PrologInterface2 pif = (PrologInterface2) pifs.get(subscription
						.getPifKey());
				if (pif != null) {
					pif.addLifeCycleHook(descr.createHook(subscription
							.getData()), descr.getHookId(), descr
							.getHookDependencies());
				}

			}
		}

	}

	
	public void removeHookDescriptor(LifeCycleHookDescriptor descr) {
		Set<LifeCycleHookDescriptor> l = hookDescriptors.get(descr.getHookId());
		if (l == null) {
			return;
		}
		if (!l.remove(descr)) {
			l.add(descr);
			Set<Subscription> subs = getSubscriptionsForTags(descr.getTags());
			for (Subscription subscription : subs) {
				PrologInterface2 pif = (PrologInterface2) pifs.get(subscription
						.getPifKey());
				if (pif != null) {
					pif.removeLifeCycleHook(descr.createHook(subscription
							.getData()), descr.getHookId());
				}

			}
		}
		if (l.isEmpty()) {
			hookDescriptors.remove(l);
		}

	}

	private Set<LifeCycleHookDescriptor> getHookDescriptorsForTags(String[] tags) {
		HashSet<LifeCycleHookDescriptor> result = new HashSet<LifeCycleHookDescriptor>();
		HashSet<String> myTags = new HashSet<String>(Arrays.asList(tags));
		Collection<HashSet<LifeCycleHookDescriptor>> descrSets = hookDescriptors
				.values();

		for (Set<LifeCycleHookDescriptor> descriptors : descrSets) {
			for (LifeCycleHookDescriptor descriptor : descriptors) {
				Set<String> descriptorTags = new HashSet<String>(Arrays
						.asList(descriptor.getTags()));
				if (!Collections.disjoint(myTags, descriptorTags)) {
					result.add(descriptor);
				}
			}
		}
		return result;
	}

	private Set<Subscription> getSubscriptionsForTags(String[] tags) {
		Set<Subscription> result = new HashSet<Subscription>();
		Set<String> myTags = new HashSet<String>(Arrays.asList(tags));

		for (Subscription subscription : subscriptions.values()) {
			Set<String> subscriptionTags = new HashSet<String>(Arrays
					.asList(subscription.getTags()));
			if (!Collections.disjoint(myTags, subscriptionTags)) {
				result.add(subscription);
			}
		}

		return result;
	}

	private String[] getTagsForSubscriptions(Set<Subscription> subscriptions) {
		HashSet<String> tags = new HashSet<String>();
		for (Subscription subscription : subscriptions) {
			tags.addAll(Arrays.asList(subscription.getTags()));
		}
		return tags.toArray(new String[tags.size()]);
	}

}

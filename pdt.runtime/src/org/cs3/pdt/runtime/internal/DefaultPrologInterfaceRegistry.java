package org.cs3.pdt.runtime.internal;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologInterfaceRegistryEvent;
import org.cs3.pdt.runtime.PrologInterfaceRegistryListener;
import org.cs3.pdt.runtime.PrologInterfaceSubscription;
import org.cs3.pl.prolog.PrologInterface;
public class DefaultPrologInterfaceRegistry implements PrologInterfaceRegistry {

	private HashMap pifs= new HashMap();
	private HashMap subscriptionLists=new HashMap();
	private HashMap names= new HashMap();
	private HashMap keys = new HashMap();
	private Vector listeners = new Vector();
	
	public Set getRegisteredKeys() {

		return pifs.keySet();
	}

	public List getSubscriptions(String key) {
		if(pifs.containsKey(key)){
			
			List l = (List) subscriptionLists.get(key);
			return l==null?new  Vector():l; 
		}
		return null;
	}

	public String getName(String key) {	
		return (String) names.get(key);
	}

	public void setName(String key, String name) {
		names.put(key,name);		
	}

	public String getKey(PrologInterface prologInterface) {
		return (String) keys.get(prologInterface);
	}

	public PrologInterface getPrologInterface(String key) {
		return (PrologInterface) pifs.get(key);
	}

	public void addPrologInterfaceRegistryListener(PrologInterfaceRegistryListener l) {
		synchronized (listeners) {
			if(!listeners.contains(l)){
				listeners.add(l);
			}
		}
		
	}

	public void removePrologInterfaceRegistryListener(PrologInterfaceRegistryListener l) {
		synchronized (listeners) {
			if(listeners.contains(l)){
				listeners.remove(l);
			}
		}
		
	}


	public void firePrologInterfaceAdded(String key){
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,key);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l= (PrologInterfaceRegistryListener) iter.next();
			l.prologInterfaceAdded(e);			
		}
	}
	
	public void firePrologInterfaceRemoved(String key){
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,key);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l= (PrologInterfaceRegistryListener) iter.next();
			l.prologInterfaceRemoved(e);			
		}
	}
	
	public void fireSubscriptionAdded(PrologInterfaceSubscription s){
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,s);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l= (PrologInterfaceRegistryListener) iter.next();
			l.subscriptionAdded(e);			
		}
	}
	
	public void fireSubscriptionRemoved(PrologInterfaceSubscription s){
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,s);
		Vector clone = null;
		synchronized (listeners) {
			clone=(Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l= (PrologInterfaceRegistryListener) iter.next();
			l.subscriptionRemoved(e);			
		}
	}

	public void addPrologInterface(String key, PrologInterface pif) {
		pifs.put(key,pif);
		keys.put(pif,key);
		firePrologInterfaceAdded(key);
	}

	public void removePrologInterface(String key) {
		List l = (List) ((Vector)subscriptionLists.get(key)).clone();
		if(l!=null){
			for (Iterator iter = l.iterator(); iter.hasNext();) {
				PrologInterfaceSubscription s = (PrologInterfaceSubscription) iter.next();
				removeSubscription(s);				
			}
		}
		subscriptionLists.remove(key);
		
		Object pif= pifs.remove(key);
		if(pif!=null){
			keys.remove(pif);		
		}
		firePrologInterfaceRemoved(key);
		names.remove(key);
		
	}

	public void addSubscription(PrologInterfaceSubscription s) {
		if(!pifs.containsKey(s.key)){
			throw new IllegalArgumentException("unkown key: "+s.key);
		}
		List l = (List) subscriptionLists.get(s.key);
		if(l==null){
			l=new Vector();
			subscriptionLists.put(s.key,l);
		}
		l.add(s);
		fireSubscriptionAdded(s);
	}

	public void removeSubscription(PrologInterfaceSubscription s) {
		if(!pifs.containsKey(s.key)){
			throw new IllegalArgumentException("unkown key: "+s.key);
		}
		List l = (List) subscriptionLists.get(s.key);
		if(l==null){
			return;
		}
		if(l.contains(s)){
			l.remove(s);
			fireSubscriptionRemoved(s);
			
		}
		
		
	}
	
	
}

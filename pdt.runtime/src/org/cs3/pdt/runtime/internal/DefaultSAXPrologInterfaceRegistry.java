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

package org.cs3.pdt.runtime.internal;

import java.io.IOException;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.cs3.pdt.runtime.PersistableSubscription;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologInterfaceRegistryEvent;
import org.cs3.pdt.runtime.PrologInterfaceRegistryListener;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.runtime.Platform;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class DefaultSAXPrologInterfaceRegistry implements PrologInterfaceRegistry {

	public void load(Reader reader) throws IOException {
		//Element cpElement;

		try {
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			//parser.setProperty("http://xml.org/sax/features/namespaces", new Boolean(true));
			//boolean b = parser.isNamespaceAware();
			parser.parse(new InputSource(reader),new RegistryHandler());
			
		} catch (SAXException e) {
			throw new _IOException(e);
		} catch (ParserConfigurationException e) {
			throw new _IOException(e);
		} finally {
			reader.close();
		}
	}
	
	class RegistryHandler extends DefaultHandler {

		public RegistryHandler() {

		}
		
		PersistableSubscription subscription;
		
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) throws SAXException {
			if (qName.equals("subscription")) {

				String bundle = attributes.getValue("bundle");
				String className = attributes.getValue("class");
				int count = attributes.getLength();
				HashMap params = new HashMap();
				for (int j = 0; j < count; j++) {
					String value = attributes.getValue(j);
					String name = attributes.getLocalName(j);
					params.put(name, value);
				}

				Class clazz;
				try {
					clazz = Platform.getBundle(bundle).loadClass(className);
					PersistableSubscription subscription = (PersistableSubscription) clazz
							.newInstance();
					subscription.restoreState(params);
					addSubscription(subscription);
				} catch (ClassNotFoundException e) {
					Debug.rethrow(e);
				} catch (IllegalAccessException e) {
					Debug.rethrow(e);
				} catch (InstantiationException e) {
					Debug.rethrow(e);
				} 
			}

		}
	}

	
	public static class _IOException extends IOException{

		/**
		 * 
		 */
		private static final long serialVersionUID = 2488611686137600862L;
		private Throwable cause;
		private String message;

		public _IOException(Throwable e) {
			this.cause = e;
			message="wrapped exception";
		}

		public _IOException(String message,Throwable e) {
			this.cause = e;
			this.message = message;
		}
		
		public Throwable fillInStackTrace() {
			return cause.fillInStackTrace();
		}

		public String getLocalizedMessage() {
			return message + " ("+cause.getLocalizedMessage()+")";
		}

		public String getMessage() {
			return message + " ("+cause.getMessage()+")";
		}

		public void printStackTrace() {
			cause.printStackTrace();
		}

		public void printStackTrace(PrintStream arg0) {
			cause.printStackTrace(arg0);
		}

		public void printStackTrace(PrintWriter arg0) {
			cause.printStackTrace(arg0);
		}

		public String toString() {
			return cause.toString();
		}
    	
    }

	public void save(Writer w) throws IOException {
		w.write("<registry>\n");
		try {
			for (Iterator it = subscriptions.values().iterator(); it.hasNext();) {
				Subscription s = (Subscription) it.next();
				PersistableSubscription ps=null;
				if (s instanceof PersistableSubscription) {
					
					ps = (PersistableSubscription) s;
				}
				if(ps!=null && ps.isPersistent()){
					Map m = ps.saveState();
					m.put("class",ps.getClass().getName());
					m.put("bundle",ps.getHostId());
					w.write("   <subscription\n");
					try{
						for (Iterator jt = m.keySet().iterator(); jt.hasNext();) {
							String key = Util.escape((String) jt.next());
							String value = Util.escape((String) m.get(key));
							w.write("      "+key+"=\""+value+"\"\n");
						}
					}
					finally{
						w.write("   />\n");
					}
				}
			}
		} finally {
			w.write("</registry>\n");
		}
	}

	private HashMap pifs = new HashMap();

	private HashMap subscriptionLists = new HashMap();

	private HashMap names = new HashMap();

	private HashMap keys = new HashMap();

	private HashMap subscriptions = new HashMap();

	private Vector listeners = new Vector();

	public Set getRegisteredKeys() {

		return pifs.keySet();
	}

	public Set getSubscriptionsForPif(String key) {
			Set l = getSubscriptionKeysForPif(key);
			Set s = new HashSet();
			for (Iterator it = l.iterator(); it.hasNext();) {
				String id = (String) it.next();
				s.add(getSubscription(id));
			}
			
		return s;
	}

	public Set getSubscriptionKeysForPif(String key) {
		Set l = (Set) subscriptionLists.get(key);
		return l == null ? new HashSet() : l;		
	}

	public String getName(String key) {
		return (String) names.get(key);
	}

	public void setName(String key, String name) {
		names.put(key, name);
	}

	public String getKey(PrologInterface prologInterface) {
		return (String) keys.get(prologInterface);
	}

	public PrologInterface getPrologInterface(String key) {
		return (PrologInterface) pifs.get(key);
	}

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
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l = (PrologInterfaceRegistryListener) iter
					.next();
			l.prologInterfaceAdded(e);
		}
	}

	public void firePrologInterfaceRemoved(String key) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				key);
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l = (PrologInterfaceRegistryListener) iter
					.next();
			l.prologInterfaceRemoved(e);
		}
	}

	public void fireSubscriptionAdded(Subscription s) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				s);
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l = (PrologInterfaceRegistryListener) iter
					.next();
			l.subscriptionAdded(e);
		}
	}

	public void fireSubscriptionRemoved(Subscription s) {
		PrologInterfaceRegistryEvent e = new PrologInterfaceRegistryEvent(this,
				s);
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologInterfaceRegistryListener l = (PrologInterfaceRegistryListener) iter
					.next();
			l.subscriptionRemoved(e);
		}
	}

	public void addPrologInterface(String key, PrologInterface pif) {
		Object old=pifs.get(key);
		if(old==pif){
			return;
		}
		if(old!=null){
			removePrologInterface(key);
		}
		pifs.put(key, pif);
		keys.put(pif, key);
		Set l = getSubscriptionsForPif(key);
		for (Iterator it = l.iterator(); it.hasNext();) {
			Subscription s = (Subscription) it.next();
			s.configure(pif);

		}
		firePrologInterfaceAdded(key);
	}

	public void removePrologInterface(String key)  {
		PrologInterface pif = (PrologInterface) pifs.get(key);
		if (pif == null) {
			return;
		}
		HashSet l =  ((HashSet) subscriptionLists.get(key));
		if (l != null) {
			l = (HashSet) l.clone();
			for (Iterator iter = l.iterator(); iter.hasNext();) {
				Subscription s = getSubscription((String) iter.next());
				s.deconfigure(pif);
			}
		}

		

		firePrologInterfaceRemoved(key);
		keys.remove(pif);
		pifs.remove(key);
		
		names.remove(key);

	}

	public void addSubscription(Subscription s){
		// do not add anonymous subscriptions
		String sid = s.getId();
		
		if (sid == null) {
			return;
		}
		Object old = subscriptions.get(sid);
		if(old==s){
			return;
		}
		if(old!=null){
			removeSubscription(sid);
		}
		Set l = (Set) subscriptionLists.get(s.getPifKey());
		if (l == null) {
			l = new HashSet();
			subscriptionLists.put(s.getPifKey(), l);
		}
		l.add(sid);
		subscriptions.put(sid, s);
		if (pifs.containsKey(s.getPifKey())) {
			s.configure(getPrologInterface(s.getPifKey()));
		}
		fireSubscriptionAdded(s);
	}

	public void removeSubscription(String id) {
		removeSubscription(getSubscription(id));
	}
	
	public void removeSubscription(Subscription s){
		// do not remove anonymous subscriptions
		if (s.getId() == null) {
			return;
		}
		if (!subscriptions.containsKey(s.getId())) {
			return;
		}
		if (pifs.containsKey(s.getPifKey())) {
			s.deconfigure(getPrologInterface(s.getPifKey()));
		}
		subscriptions.remove(s.getId());

		Set l = (Set) subscriptionLists.get(s.getPifKey());
		if (l == null) {
			return;
		}
		if (l.contains(s.getId())) {
			l.remove(s.getId());
			fireSubscriptionRemoved(s);

		}

	}

	public Subscription getSubscription(String key) {
		return (Subscription) subscriptions.get(key);

	}

	public Set getAllKeys() {
		Set s = new HashSet(getRegisteredKeys());
		s.addAll(subscriptionLists.keySet());
		return s;
	}

	public Set getAllSubscriptionIDs() {
		return new HashSet(subscriptions.keySet());
	}

}

package org.cs3.pdt.runtime.internal;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.cs3.pdt.runtime.PersistableSubscription;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologInterfaceRegistryEvent;
import org.cs3.pdt.runtime.PrologInterfaceRegistryListener;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.Platform;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

public class DefaultPrologInterfaceRegistry implements PrologInterfaceRegistry {

	public void load(Reader reader) throws IOException {
		Element cpElement;

		try {
			DocumentBuilder parser = DocumentBuilderFactory.newInstance()
					.newDocumentBuilder();
			cpElement = parser.parse(new InputSource(reader))
					.getDocumentElement();
		} catch (SAXException e) {
			IOException ee = new IOException();
			ee.initCause(e);
			throw ee;
		} catch (ParserConfigurationException e) {
			IOException ee = new IOException();
			ee.initCause(e);
			throw ee;
		} finally {
			reader.close();
		}

		if (!cpElement.getNodeName().equalsIgnoreCase("registry")) { //$NON-NLS-1$
			throw new IOException("expected <registry>, but was <"
					+ cpElement.getNodeName() + ">");
		}
		NodeList list = cpElement.getElementsByTagName("subscription"); //$NON-NLS-1$

		int length = list.getLength();

		for (int i = 0; i < length; ++i) {
			Node node = list.item(i);
			if (node.getNodeType() == Node.ELEMENT_NODE) {
				NamedNodeMap attributes = node.getAttributes();
				String bundle = attributes.getNamedItem("bundle")
						.getNodeValue();
				String className = attributes.getNamedItem("class")
						.getNodeValue();
				int count = attributes.getLength();
				HashMap params = new HashMap();
				for (int j = 0; j < length; j++) {
					Node parmNode = attributes.item(j);
					params.put(parmNode.getNodeName(), parmNode.getNodeValue());
				}

				try {
					Class clazz = Platform.getBundle(bundle).loadClass(
							className);
					PersistableSubscription subscription = (PersistableSubscription) clazz
							.newInstance();
					subscription.restoreState(params);
					addSubscription(subscription);
				} catch (ClassNotFoundException e) {
					IOException ee = new IOException();
					ee.initCause(e);
					throw ee;
				} catch (IllegalAccessException e) {
					IOException ee = new IOException();
					ee.initCause(e);
					throw ee;
				} catch (InstantiationException e) {
					IOException ee = new IOException();
					ee.initCause(e);
					throw ee;
				} catch (ClassCastException e) {
					IOException ee = new IOException();
					ee.initCause(e);
					throw ee;
				}

			}
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

	public List getSubscriptionsForPif(String key) {
		if (pifs.containsKey(key)) {

			List l = (List) subscriptionLists.get(key);
			return l == null ? new Vector() : l;
		}
		return null;
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
		pifs.put(key, pif);
		keys.put(pif, key);
		List l = getSubscriptionsForPif(key);
		for (Iterator it = l.iterator(); it.hasNext();) {
			Subscription s = (Subscription) it.next();
			s.configure(pif);

		}
		firePrologInterfaceAdded(key);
	}

	public void removePrologInterface(String key) {
		PrologInterface pif = (PrologInterface) pifs.get(key);
		if (pif == null) {
			return;
		}
		List l = (List) ((Vector) subscriptionLists.get(key)).clone();
		if (l != null) {
			for (Iterator iter = l.iterator(); iter.hasNext();) {
				Subscription s = (Subscription) iter.next();
				s.deconfigure(pif);
			}
		}

		pifs.remove(key);

		firePrologInterfaceRemoved(key);
		names.remove(key);

	}

	public void addSubscription(Subscription s) {
		// do not add anonymous subscriptions
		if (s.getId() == null) {
			return;
		}
		List l = (List) subscriptionLists.get(s.getPifKey());
		if (l == null) {
			l = new Vector();
			subscriptionLists.put(s.getPifKey(), l);
		}
		l.add(s);
		subscriptions.put(s.getId(), s);
		if (pifs.containsKey(s.getPifKey())) {
			s.configure(getPrologInterface(s.getPifKey()));
		}
		fireSubscriptionAdded(s);
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
		}
		subscriptions.remove(s.getId());

		List l = (List) subscriptionLists.get(s.getPifKey());
		if (l == null) {
			return;
		}
		if (l.contains(s)) {
			l.remove(s);
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

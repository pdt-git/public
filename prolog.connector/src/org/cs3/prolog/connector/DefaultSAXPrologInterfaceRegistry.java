/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.connector;

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.eclipse.core.runtime.Platform;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.helpers.DefaultHandler;

public class DefaultSAXPrologInterfaceRegistry extends DefaultPrologInterfaceRegistry {

	@Override
	public void load(Reader reader) throws IOException {
		//Element cpElement;

		try {
			SAXParser parser = SAXParserFactory.newInstance().newSAXParser();
			//parser.setProperty("http://xml.org/sax/features/namespaces", new Boolean(true));
			//boolean b = parser.isNamespaceAware();
			parser.parse(new InputSource(reader),new RegistryHandler());
			
		} catch (SAXException e) {
			// TODO: changed this to new IOException(e) once Java 6 is supported on all target platforms
			Debug.report(e);
			throw new IOException(e.getLocalizedMessage());
		} catch (ParserConfigurationException e) {
			// TODO: changed this to new IOException(e) once Java 6 is supported on all target platforms
			Debug.report(e);
			throw new IOException(e.getLocalizedMessage());
		} finally {
			reader.close();
		}
	}
	
	class RegistryHandler extends DefaultHandler {

		public RegistryHandler() {

		}
		
		PersistableSubscription subscription;
		
		@Override
		public void startElement(String uri, String localName, String qName,
				Attributes attributes) throws SAXException {
			if (qName.equals("subscription")) {

				String bundle = attributes.getValue("bundle");
				String className = attributes.getValue("class");
				int count = attributes.getLength();
				HashMap<String, String> params = new HashMap<String, String>();
				for (int j = 0; j < count; j++) {
					String value = attributes.getValue(j);
					String name = attributes.getLocalName(j);
					params.put(name, value);
				}

				Class<?> clazz;
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

	
	
	@Override
	public void save(Writer w) throws IOException {
		w.write("<registry>\n");
		try {
					
			for (Subscription s : getAllSubscriptions()) {
				
			
				PersistableSubscription ps=null;
				if (s instanceof PersistableSubscription) {
					
					ps = (PersistableSubscription) s;
				}
				if(ps!=null && ps.isPersistent()){
					Map<String, String> m = ps.saveState();
					m.put("class",ps.getClass().getName());
					m.put("bundle",ps.getHostId());
					w.write("   <subscription\n");
					try{
						for (Iterator<String> jt = m.keySet().iterator(); jt.hasNext();) {
							String key = Util.escape(jt.next());
							String value = Util.escape(m.get(key));
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

	

}


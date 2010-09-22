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

import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
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

				@SuppressWarnings("rawtypes")
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

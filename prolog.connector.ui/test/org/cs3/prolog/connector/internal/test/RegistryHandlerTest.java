/* $LICENSE_MSG$ */

package org.cs3.prolog.connector.internal.test;

import java.io.Reader;
import java.io.StringReader;

import junit.framework.TestCase;

import org.cs3.prolog.connector.DefaultSAXPrologInterfaceRegistry;

public class RegistryHandlerTest extends TestCase {
	public void testHandler() throws Exception {
		Reader reader = new StringReader(
				"<registry>"+
				"   <subscription"+
				"      hostid=\"org.cs3.pdt.core\""+
				"      persistent=\"true\""+
				"      class=\"org.cs3.pdt.core.internal.natures.RuntimeSubscription\""+
				"      pifkey=\"JTEngine\""+
				"      description=\"used as default runtime for projectJTEngine\""+
				"      project=\"JTEngine\""+
				"      bundle=\"org.cs3.pdt.core\""+
				"      name=\"JTEngine - runtime\""+
				"      id=\"JTEngine.runtime_subscription\""+
				"   />"+
				"   <subscription"+
				"      hostid=\"org.cs3.pdt.core\""+
				"      persistent=\"true\""+
				"      class=\"org.cs3.pdt.core.internal.natures.MetadataSubscription\""+
				"      pifkey=\"JTEngine\""+
				"      description=\"used to store and process meta information on prologsource code found in project JTEngine\""+
				"      project=\"JTEngine\""+
				"      bundle=\"org.cs3.pdt.core\""+
				"      name=\"JTEngine - metadata\""+
				"      id=\"JTEngine.metadata_subscription\""+
				"   />"+
				"</registry>");
			new DefaultSAXPrologInterfaceRegistry().load(reader);		
		}
}


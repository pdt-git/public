/* $LICENSE_MSG$(ld) */

package org.cs3.pdt;

import junit.framework.TestCase;

import org.cs3.prolog.common.Util;

public class PDTUtilsTest extends TestCase {

	public void testLogicalToPhysicalOffset01() throws Throwable{
		String data = "0"+"\r\n"+"2"+"3"+"\n"+"5";
		assertEquals(0,Util.logicalToPhysicalOffset(data,0));
		assertEquals(1,Util.logicalToPhysicalOffset(data,1));
		assertEquals(3,Util.logicalToPhysicalOffset(data,2));
		assertEquals(4,Util.logicalToPhysicalOffset(data,3));
		assertEquals(5,Util.logicalToPhysicalOffset(data,4));
		assertEquals(6,Util.logicalToPhysicalOffset(data,5));
		
	}
}


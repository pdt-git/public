package org.cs3.pdt;

import org.eclipse.jdt.internal.core.util.SimpleDocument;

import junit.framework.TestCase;

public class PDTUtilsTest extends TestCase {

	public void testLogicalToPhysicalOffset01() throws Throwable{
		String data = "0"+"\r\n"+"2"+"3"+"\n"+"5";
		assertEquals(0,PDTUtils.logicalToPhysicalOffset(data,0));
		assertEquals(1,PDTUtils.logicalToPhysicalOffset(data,1));
		assertEquals(3,PDTUtils.logicalToPhysicalOffset(data,2));
		assertEquals(4,PDTUtils.logicalToPhysicalOffset(data,3));
		assertEquals(5,PDTUtils.logicalToPhysicalOffset(data,4));
		assertEquals(6,PDTUtils.logicalToPhysicalOffset(data,5));
		
	}
}

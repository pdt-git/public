/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.common.test;

import org.cs3.prolog.common.Util;

import junit.framework.TestCase;

public class UtilTest extends TestCase {
	
	public void testQuoteAtom(){
		String atom="something('something else')";
		assertEquals("'something(\\'something else\\')'", Util.quoteAtom(atom));
	}
	
	public void testHideStreamHandles(){
		String s="something(to($stream(hid976)))$stream(562)";
		assertEquals("something(to($stream(_)))$stream(_)", Util.hideStreamHandles(s, "$stream(_)"));
	}
	
	public void testLogicalToPhysicalOffset01() throws Throwable{
		byte[] bytes={0x30,0x0D,0x0a,0x32,0x0a,0x35};
		String data = new String(bytes);
		assertEquals(0,Util.logicalToPhysicalOffset(data,0));
		assertEquals(1,Util.logicalToPhysicalOffset(data,1));
		assertEquals(3,Util.logicalToPhysicalOffset(data,2));
		assertEquals(4,Util.logicalToPhysicalOffset(data,3));
		assertEquals(5,Util.logicalToPhysicalOffset(data,4));
		assertEquals(6,Util.logicalToPhysicalOffset(data,5));
	}
}


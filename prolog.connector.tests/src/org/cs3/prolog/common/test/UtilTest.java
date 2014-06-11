/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.common.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.common.Util;

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



/*
 * Created on 28.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.views;

import junit.framework.TestCase;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PrologMessageTest extends TestCase {
	
	public void testGetReferenceLine() {
		PrologMessage msg = new PrologMessage("asdfaf.pl:10:asdfsadf", 1, PrologMessage.ERROR);
		assertEquals(msg.getReferencedLine(),10);
		msg = new PrologMessage("asdfaf.pl:4:asdfsadf", 1, PrologMessage.ERROR);
		assertEquals(msg.getReferencedLine(),4);
	}
	
	public void testGetReferenceFile() {
		PrologMessage msg = new PrologMessage("ERROR: z:/work/runtime-workspace/prologtest/test2.pl:4: Syntax error: Operator expected	", 1, PrologMessage.ERROR);
		assertEquals(msg.getReferencedFilename(),"z:/work/runtime-workspace/prologtest/test2.pl");
		msg = new PrologMessage("WARNING: z:/work/runtime-workspace/prologtest/test2.pl:4: Syntax error: Operator expected	", 1, PrologMessage.WARNING);
		assertEquals(msg.getReferencedFilename(),"z:/work/runtime-workspace/prologtest/test2.pl");
		msg = new PrologMessage("ERROR: (z:/work/runtime-workspace/prologtest/test2.pl:4:) Syntax error: Operator expected	", 1, PrologMessage.ERROR);
		assertEquals(msg.getReferencedFilename(),"z:/work/runtime-workspace/prologtest/test2.pl");
	}
		

}

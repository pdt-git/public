/*
 * Created on 02.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.editors;

import org.cs3.pl.metadata.PrologElementData;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;

import junit.framework.TestCase;

/**
 * @author windeln
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PLEditorTest extends TestCase {
	public void testGetPrologDataFromOffset() throws BadLocationException {
		Document document = new Document("  , ahaha(a,b,c).");
		PrologElementData data = PLEditor.getPrologDataFromOffset(document, 6);
		assertEquals("ahaha",data.getLabel());
		assertEquals(3,data.getArity());

		document = new Document("  , ahah1a(a,b(a),c).");
		data = PLEditor.getPrologDataFromOffset(document, 6);
		assertEquals("ahah1a",data.getLabel());
		assertEquals(3,data.getArity());

		document = new Document("  , ahaha(\"asdf\\\"\'\",b(a),c).");
		data = PLEditor.getPrologDataFromOffset(document, 6);
		assertEquals("ahaha",data.getLabel());
		assertEquals(3,data.getArity());
	
		document = new Document("  , ahaha(\"as,df\\\"\'\",b(a),[c,b]).");
		data = PLEditor.getPrologDataFromOffset(document, 6);
		assertEquals("ahaha",data.getLabel());
		assertEquals(3,data.getArity());

		document = new Document("  , aha_ha(\"as,df\\\"\'\",b(a),[c,b]).");
		data = PLEditor.getPrologDataFromOffset(document, 6);
		assertEquals("aha_ha",data.getLabel());
		assertEquals(3,data.getArity());

		document = new Document(" test/12");
		data = PLEditor.getPrologDataFromOffset(document, 3);
		assertEquals("test",data.getLabel());
		assertEquals(12,data.getArity());

		document = new Document("type: ");
		data = PLEditor.getPrologDataFromOffset(document, 3);
		assertEquals("type",data.getLabel());
		assertEquals(-1,data.getArity());
	}
}

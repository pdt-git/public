/*
 * Created on 20.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.editors;


import org.cs3.pl.common.Debug;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultAutoIndentStrategy;
import org.eclipse.jface.text.DocumentCommand;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextUtilities;
/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class PLAutoIndentStrategy extends DefaultAutoIndentStrategy {

	private void autoIndentAfterNewLine(IDocument d, DocumentCommand c) {
		
		if (c.offset == -1 || d.getLength() == 0)
			return;
		
		try {
			// find start of line
			int p= (c.offset == d.getLength() ? c.offset  - 1 : c.offset);
			IRegion info= d.getLineInformationOfOffset(p);
			
			int start= info.getOffset();
					
			if ((c.offset == start + info.getLength()) && isPredicateHead(d,info)) 
				predicateIndent(c);
			
			// find white spaces
			int end= findEndOfWhiteSpace(d, start, c.offset);
			
			StringBuffer buf= new StringBuffer(c.text);
			if (end > start) {			
				// append to input
				buf.append(d.get(start, end - start));
			}
			
			c.text= buf.toString();
			
		} catch (BadLocationException excp) {
			// stop work
		}	
	}
	
	/**
	 * 
	 */
	private void predicateIndent(DocumentCommand c) {
		c.text = "\n    ";
		
	}

	/**
	 * @param info
	 * @return
	 */
	private boolean isPredicateHead(IDocument d, IRegion info) {
		String line;
		try {
			line = d.get(info.getOffset(), info.getLength());
			String removedWS =line.replaceAll(" ", "");
			return removedWS.endsWith(":-");
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		return false;
	}

	/*
	 * @see IAutoIndentStrategy#customizeDocumentCommand
	 */
	public void customizeDocumentCommand(IDocument d, DocumentCommand c) {
		if (c.length == 0 && c.text != null && TextUtilities.endsWith(d.getLegalLineDelimiters(), c.text) != -1)
			autoIndentAfterNewLine(d, c);
	}
	
}

/*
 */
package org.cs3.pdt.internal.editors;

import org.cs3.pl.parser.LineBreakInfoProvider;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

/**
 */
public class DocumentLineBreakInfoProvider implements LineBreakInfoProvider {
    private IDocument document;

    public DocumentLineBreakInfoProvider(IDocument document) {
        this.document = document;
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getLineAtOffset(int)
     */
    public int getLineAtOffset(int offset) {
        try {
            return document.getLineOfOffset(offset);
        } catch (BadLocationException e) {
	        throw new IndexOutOfBoundsException("invalid offset: "+offset);
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getOffsetAtLine(int)
     */
    public int getOffsetAtLine(int line) {
        try {
            return document.getLineOffset(line);
        } catch (BadLocationException e) {
            throw new IndexOutOfBoundsException("invalid line number: "+line);
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getLineCount()
     */
    public int getLineCount() {
        return document.getNumberOfLines();
    }

}

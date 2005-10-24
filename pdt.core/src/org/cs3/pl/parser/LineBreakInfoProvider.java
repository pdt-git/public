package org.cs3.pl.parser;

/**
 * Simple helper that knows about the position of all line breaks within e.g.
 * some character stream.
 */
public interface LineBreakInfoProvider {
    /**
     * @return the offset at which the given line starts.
     * The offset at 0 is always 0. For a line index >0 this method should return
     * the offset of the first character in this line.
     */
    public int getOffsetAtLine(int line);
    
    public int getLineAtOffset(int offset);
    
    
    /**
     * @return the total number of lines.
     */
    public int getLineCount();
}
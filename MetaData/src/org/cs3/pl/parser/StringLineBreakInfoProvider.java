/*
 */
package org.cs3.pl.parser;

import java.util.Vector;

/**
 */
public class StringLineBreakInfoProvider implements LineBreakInfoProvider {
    Vector lineBreaks = new Vector();

    public StringLineBreakInfoProvider(String text) {

        String nl = System.getProperty("line.separator");
        int offset = text.indexOf(nl);
        while (offset >= 0 && offset <= text.length()) {
            lineBreaks.add(new Integer(offset));
            offset = text.indexOf(nl, offset + 1);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getOffsetAtLine(int)
     */
    public int getOffsetAtLine(int line) {
        return line == 0 ? 0 : ((Integer) lineBreaks.get(line - 1)).intValue()+1;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.parser.LineBreakInfoProvider#getLineCount()
     */
    public int getLineCount() {
        return lineBreaks.size();
    }

}
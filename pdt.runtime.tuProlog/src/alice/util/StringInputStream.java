/*
 *   StringInputStream.java
 *
 * Copyright 2000-2001-2002  aliCE team at deis.unibo.it
 *
 * This software is the proprietary information of deis.unibo.it
 * Use is subject to license terms.
 *
 */
package alice.util;
import java.io.IOException;
import java.io.InputStream;

/**
 *
 * managing a String or other InputStream as
 * a character source
 */
public class StringInputStream extends InputStream {

    /** alternative input source */
    private String      source;

    /** current char pos on stream */
    private int pos;

    /** current line on stream */
    private int line;

    public StringInputStream(String s) {
        source      = s;
        pos=0;
        line=1;
    }

    /** read a char */
    public int read() throws IOException {
        if (pos<source.length()){
            int c       = source.charAt(pos);
            pos++;
            if (((char)c)=='\n'){
                line++;
            }
            return(c);
        } else {
            return -1;
        }
    }

    public void reset() throws IOException {
        pos=0;
        line=1;
    }

    /** go to n-th char */
    public void seek(int n){
        pos=n;
        if (pos>source.length()){
            pos=source.length();
        }
    }

    public int getCurrentPos(){
        return pos;
    }

    public int getCurrentLine(){
        return line;
    }
}

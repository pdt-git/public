/*
 */
package org.cs3.pl.common;

import java.io.PrintStream;

/**
 */
public class SimpleLogBuffer implements LogBuffer {

    private StringBuffer buf=new StringBuffer();
    private String lastKey=null;
    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, char)
     */
    public void log(String key, char c) {
        setKey(key);
        buf.append(c);        
    }

    /**
     * @param key
     */
    private void setKey(String key) {
        if(lastKey==null&& key!=null
                || ! lastKey.equals(key)){
            buf.append("</"+lastKey+">\n<"+key+">");
            lastKey=key;
        }
        
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, char[], int, int)
     */
    public void log(String key, char[] buf, int offset, int len) {
        setKey(key);
        this.buf.append(buf,offset,len);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[], int, int)
     */
    public void log(String key, byte[] buf, int offset, int len) {
        setKey(key);
        if(len>0)
        {
            this.buf.append(new String(buf,offset,len));
        }
        else{
            this.buf.append("<<EOF>>");
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, java.lang.String)
     */
    public void log(String key, String s) {
        setKey(key);
        buf.append(s);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[])
     */
    public void log(String key, byte[] b) {
        setKey(key);
        buf.append(b);
    }

    

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#printLog(java.io.PrintStream)
     */
    public void printLog(PrintStream out) {
       out.println(buf.toString());        
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public String toString() {     
        return buf.toString();
    }
}

/*
 */
package org.cs3.pl.common;

import java.io.PrintStream;

/**
 */
public class SimpleLogBuffer implements LogBuffer {
    private final static int MAX_LENGTH=500000;//this should be enough. 
        private StringBuffer buf=new StringBuffer();
    private String lastKey=null;
    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, char)
     */
    public void log(String key, char c) {
        setKey(key);
        buf.append(c);        
        cutHead();
    }

    private void cutHead(){
        int cut = buf.length()-MAX_LENGTH;
        if(cut>0){
            buf.delete(0,Math.min(cut*2,buf.length()+1/2));
        }
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
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, char[], int, int)
     */
    public void log(String key, char[] buf, int offset, int len) {
        setKey(key);
        this.buf.append(buf,offset,len);
        cutHead();
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
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, java.lang.String)
     */
    public void log(String key, String s) {
        setKey(key);
        buf.append(s);
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[])
     */
    public void log(String key, byte[] b) {
        setKey(key);
        buf.append(b);
        cutHead();
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
        cutHead();
        return buf.toString();
    }
}

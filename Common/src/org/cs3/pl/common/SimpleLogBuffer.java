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
    public synchronized void log(String key, char c) {
        log(key,new byte[]{(byte) c});
    }

    private synchronized void cutHead(){
        int cut = buf.length()-MAX_LENGTH;
        if(cut>0){
            buf.delete(0,Math.min(cut*2,buf.length()+1/2));
        }
    }
    
    /**
     * @param key
     */
    private synchronized void setKey(String key) {
		if(lastKey!=null){
			buf.append("</"+lastKey+">\n");
		}
		
		if(lastKey==null&& key!=null
                || ! lastKey.equals(key)){
			buf.append("<"+key+">");
            lastKey=key;
        }			
        cutHead();
    }

    

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[], int, int)
     */
    public synchronized void log(String key, byte[] buf, int offset, int len) {
        setKey(key);
        if(len>0)
        {
            String string = new String(buf,offset,len);
			this.buf.append(string);
			//Debug.debug(key+": "+string);
        }
        else{
            this.buf.append("<<EOF>>");
			//Debug.debug(key+": <<EOF>>");
        }
        cutHead();
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, java.lang.String)
     */
    public synchronized void log(String key, String s) {
        byte[] bytes = s.getBytes();
		log(key,bytes,0,bytes.length);		
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#log(java.lang.String, byte[])
     */
    public synchronized void log(String key, byte[] b) {
		log(key,b,0,b.length);
    }

    

    /* (non-Javadoc)
     * @see org.cs3.pl.common.LogBuffer#printLog(java.io.PrintStream)
     */
    public synchronized void printLog(PrintStream out) {
       out.println(buf.toString());        
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    public synchronized String toString() {     
        cutHead();
        return buf.toString();
    }
}

/*
 */
package org.cs3.pl.common;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 */
public class InputStreamSpy extends BufferedInputStream {

    private OutputStream spy;
    private long mypos=0;
    

    /**
     * @return Returns the mypos.
     */
    public long getMypos() {
        return mypos;
    }
    /**
     * 
     */
    public InputStreamSpy(InputStream src, OutputStream spy) {
        super(src);
        this.spy=spy;
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#read()
     */
    public int read() throws IOException {
        int r = super.read();
        if(r!=-1){
            spy.write(r);
            spy.flush();
            mypos+=r;
        }
        return r;
    }

    /* (non-Javadoc)
     * @see java.io.InputStream#read(byte[], int, int)
     */
    public synchronized int read(byte[] b, int off, int len) throws IOException {
       
        int r= super.read(b, off, len);
        if(r!=-1){
            spy.write(b,off,r);
            spy.flush();
            mypos+=r;
        }      
        return r;
    }
    /* (non-Javadoc)
     * @see java.io.InputStream#read(byte[])
     */
    public int read(byte[] b) throws IOException {
      int r= super.read(b);
      if(r!=-1){
          spy.write(b,0,r);
          spy.flush();
          mypos+=r;
      }
      return r;
    }
    /* (non-Javadoc)
     * @see java.io.InputStream#skip(long)
     */
    public synchronized long skip(long n) throws IOException {
        // TODO Auto-generated method stub
        long l = super.skip(n);
        mypos +=l;
        return l;
    }
}

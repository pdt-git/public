package org.cs3.pl.common;

import java.io.IOException;
import java.io.InputStream;

public class LoggingInputStream extends InputStream{
	InputStream impl;
	LogBuffer log;
	private String key="read";
	

	public LoggingInputStream(InputStream impl, LogBuffer log, String key) {
		super();
		this.impl = impl;
		this.log = log;
		this.key = key;
	}
	public LoggingInputStream(InputStream impl, LogBuffer log) {
		super();
		this.impl = impl;
		this.log = log;	
	}

	
	
	
	public int available() throws IOException {
		int i = impl.available();
		//log.log(key+":available",""+i);
		return i;
	}
	public boolean markSupported() {
		boolean b = impl.markSupported();
		//log.log(key+":available",""+b);		
		return b;
	}
	public void close() throws IOException {
		log.log(key+":close","closing target.");
		impl.close();
	}

	public synchronized void mark(int readlimit) {
		log.log(key+":mark","readlimit="+readlimit);
		impl.mark(readlimit);
	}


	public int read() throws IOException {
		int i = impl.read();
		if(i==-1){
			log.log(key,"##EOF##");
		}else{
			log.log(key,(char)i);
		}
		return i;
	}

	public int read(byte[] b, int off, int len) throws IOException {
		int i = impl.read(b, off, len);
		log.log(key,b,off,i);
		return i;
	}

	public int read(byte[] b) throws IOException {
		int i= impl.read(b);
		log.log(key,b,0,i);
		return i;
	}

	public synchronized void reset() throws IOException {
		log.log(key+":reset","reseting target");
		impl.reset();
	}

	public long skip(long n) throws IOException {

		long remaining = n;
		int nr;
		

		int bufsize = (int)Math.min(Integer.MAX_VALUE, n);
		byte[] localSkipBuffer = new byte[bufsize];
			
		if (n <= 0) {
		    return 0;
		}

		while (remaining > 0) {
		    nr = read(localSkipBuffer, 0,
			      (int) Math.min(bufsize, remaining));
		    if (nr < 0) {
			break;
		    }
		    remaining -= nr;
		}
		
		int read = (int) (n - remaining);
		log.log(key+":skip",localSkipBuffer,0,read);
		return read;
	}

	

}

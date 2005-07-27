package org.cs3.pl.common;

import java.io.IOException;
import java.io.OutputStream;

public class LoggingOutputStream extends OutputStream {
	OutputStream impl;
	LogBuffer log;
	private String key="write";
	public void close() throws IOException {
		log.log(key+":close","closing target.");
		impl.close();
	}

	public void flush() throws IOException {
		//log.log(key+":flush","flushing target.");
		impl.flush();
	}

	public void write(byte[] b, int off, int len) throws IOException {
		log.log(key,b,off,len);
		impl.write(b, off, len);
	}

	public void write(byte[] b) throws IOException {
		log.log(key,b);
		impl.write(b);
	}

	public void write(int b) throws IOException {
		log.log(key,(char)b);
		impl.write(b);
	}

	public LoggingOutputStream(OutputStream impl, LogBuffer log, String key) {
		super();		
		this.impl = impl;
		this.log = log;
		this.key = key;
	}
	public LoggingOutputStream(OutputStream impl, LogBuffer log) {
		super();		
		this.impl = impl;
		this.log = log;
		
	}
}

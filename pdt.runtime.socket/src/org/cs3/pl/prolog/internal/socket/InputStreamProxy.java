package org.cs3.pl.prolog.internal.socket;

import java.io.IOException;
import java.io.InputStream;

import org.cs3.pl.common.LogBuffer;

class InputStreamProxy extends InputStream {
	private InputStream in;
	private SocketClient client;

	private LogBuffer logBuf;


	private InputStreamProxy(InputStream in, LogBuffer logBuf) {
		super();
		this.in = in;
		this.logBuf = logBuf;
	}

	public InputStreamProxy(InputStream in, LogBuffer logBuf, SocketClient client) {
		this(in,logBuf);
		this.client = client;
	}

	public int available() throws IOException {
		return in.available();
	}

	public void close() throws IOException {
		client.close();
	}

	public synchronized void mark(int readlimit) {
		in.mark(readlimit);
	}

	public boolean markSupported() {
		return in.markSupported();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see java.io.InputStream#read()
	 */
	public int read() throws IOException {
		int read = in.read();
		logBuf.log("read", (char) read);
		return read;
	}

	public int read(byte[] b) throws IOException {
		int read = in.read(b);
		logBuf.log("read", b, 0, read);
		return read;
	}

	public int read(byte[] b, int off, int len) throws IOException {
		int read = in.read(b, off, len);
		logBuf.log("read", b, off, read);
		return read;
	}

	public synchronized void reset() throws IOException {
		in.reset();
	}

	public long skip(long n) throws IOException {
		return in.skip(n);
	}

}
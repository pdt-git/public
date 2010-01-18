package org.cs3.pl.prolog.internal.socket;

import java.io.IOException;
import java.io.OutputStream;

import org.cs3.pl.common.LogBuffer;

class OutputStreamProxy extends OutputStream {
	private OutputStream out;
	private SocketClient client;

	private LogBuffer logBuf;


	private OutputStreamProxy(OutputStream out, LogBuffer logBuf) {
		super();
		this.out = out;
		this.logBuf = logBuf;
	}

	public OutputStreamProxy(OutputStream out, LogBuffer logBuf, SocketClient client) {
		this(out,logBuf);
		this.client = client;
	}

	public void close() throws IOException {
		client.close();
	}

	public void flush() throws IOException {
		out.flush();
	}

	public void write(byte[] b) throws IOException {
		out.write(b);
		logBuf.log("write", b);
	}

	public void write(byte[] b, int off, int len) throws IOException {
		out.write(b, off, len);
		logBuf.log("write", b, off, len);
	}

	public void write(int b) throws IOException {
		out.write(b);
		logBuf.log("write", (char) b);
	}
}

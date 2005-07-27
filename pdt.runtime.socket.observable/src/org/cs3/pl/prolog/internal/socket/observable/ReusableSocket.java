/*
 */
package org.cs3.pl.prolog.internal.socket.observable;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketImpl;
import java.net.UnknownHostException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.LogBuffer;
import org.cs3.pl.common.LoggingInputStream;
import org.cs3.pl.common.LoggingOutputStream;
import org.cs3.pl.common.SimpleLogBuffer;
import org.cs3.pl.prolog.internal.Reusable;

/**
 */
public class ReusableSocket extends Socket implements Reusable {

    private LogBuffer logBuffer = new SimpleLogBuffer();
	private LoggingInputStream myInput;
	private OutputStream myOutput;

    public InputStream getInputStream() throws IOException {
		if (isClosed())
		    throw new SocketException("Socket is closed");
		if (!isConnected())
		    throw new SocketException("Socket is not connected");
		if (isInputShutdown())
		    throw new SocketException("Socket input is shutdown");
		if(myInput==null){
			myInput= new LoggingInputStream(super.getInputStream(),logBuffer);
		}
		return myInput;
	}

	public OutputStream getOutputStream() throws IOException {
		if (isClosed())
		    throw new SocketException("Socket is closed");
		if (!isConnected())
		    throw new SocketException("Socket is not connected");
		if (isOutputShutdown())
		    throw new SocketException("Socket output is shutdown");
		if(myOutput==null){
			myOutput= new LoggingOutputStream(super.getOutputStream(),logBuffer);
		}
		return myOutput;		
	}

	/**
     * 
     */
    public ReusableSocket() {
        super();
        // TODO Auto-generated constructor stub
    }

    /**
     * @param host
     * @param port
     * @throws java.net.UnknownHostException
     * @throws java.io.IOException
     */
    public ReusableSocket(String host, int port) throws UnknownHostException,
            IOException {
        super(host, port);
    }

    

    /**
     * @param address
     * @param port
     * @throws java.io.IOException
     */
    public ReusableSocket(InetAddress address, int port) throws IOException {
        super(address, port);
    }

   

    /**
     * @param impl
     * @throws java.net.SocketException
     */
    public ReusableSocket(SocketImpl impl) throws SocketException {
        super(impl);
    }

    /**
     * @param host
     * @param port
     * @param localAddr
     * @param localPort
     * @throws java.io.IOException
     */
    public ReusableSocket(String host, int port, InetAddress localAddr,
            int localPort) throws IOException {
        super(host, port, localAddr, localPort);
    }

    /**
     * @param address
     * @param port
     * @param localAddr
     * @param localPort
     * @throws java.io.IOException
     */
    public ReusableSocket(InetAddress address, int port, InetAddress localAddr,
            int localPort) throws IOException {
        super(address, port, localAddr, localPort);
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#reuse()
     */
    public void reuse() {
        logBuffer.log("socket","reuse");;
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#destroy()
     */
    public void destroy() {
        logBuffer.log("socket","destroy");
        try {
            close();
        } catch (IOException e) {
	        Debug.report(e);
	        throw new RuntimeException(e);
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.Reusable#recylce()
     */
    public void recylce() {
        logBuffer.log("socket","recycle");
    }

    /**
     * @return
     */
    public LogBuffer getLogBuffer() {
        return this.logBuffer;
    }

}

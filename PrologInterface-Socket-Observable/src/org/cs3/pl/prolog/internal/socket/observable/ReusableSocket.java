/*
 */
package org.cs3.pl.prolog.internal.socket.observable;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.SocketException;
import java.net.SocketImpl;
import java.net.UnknownHostException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.LogBuffer;
import org.cs3.pl.common.SimpleLogBuffer;
import org.cs3.pl.prolog.internal.Reusable;

/**
 */
public class ReusableSocket extends Socket implements Reusable {

    private LogBuffer logBuffer = new SimpleLogBuffer();

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

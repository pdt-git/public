/*--------------------------------------------------------------------------*
 | Copyright (C) 2001 Christopher Kohlhaas                                  |
 |                                                                          |
 | This program is free software; you can redistribute it and/or modify     |
 | it under the terms of the GNU General Public License as published by the |
 | Free Software Foundation. A copy of the license has been included with   |
 | these distribution in the COPYING file, if not go to www.fsf.org         |
 |                                                                          |
 | As a special exception, you are granted the permissions to link this     |
 | program with every library, which license fulfills the Open Source       |
 | Definition as published by the Open Source Initiative (OSI).             |
 *--------------------------------------------------------------------------*/
package org.rapla.components.rpc;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.lang.reflect.InvocationTargetException;
import java.net.ConnectException;
import java.net.Socket;
import java.net.SocketException;
import java.security.Provider;
import java.security.Security;
import java.util.ArrayList;
import java.util.HashMap;

/** Messaging component that is used on the client side.
   The port and server value specify the address of the
   MessagingClient. If secure-sockets is set an ssl-connection will be
   used for communicating with the server. This requires a server
   that provides an ssl-connection on the specified port
   {@link RaplaSSLSocketFactory}
   See the source-code of TestRemoteClient for an example client.
  @see MessagingServer
*/
public class MessagingClientImpl
    implements
        MessagingClient
{
    private ArrayList listenerList = new ArrayList();
    public static int SOCKET_CONNECT_TIMEOUT = 30000;
    public static int SOCKET_ALIVE_TIMEOUT = 30000;

    Socket socket;
    Object dispatcher;
    private boolean running = false;
    private boolean closing = false;
    protected String server;
    protected int port = -1;
    protected boolean secureSockets;
    EventSink inputThread;
    EventSource outputThread;
    ObjectInputStream input;
    ObjectOutputStream output;
    HashMap callbacks = new HashMap();

    private Mutex connectLock = createMutex("connect"); // used to synchronize the connection states  
    private Mutex callLock = createMutex("call"); // only one call should be processed during the time
    
    public void setServer(String server) {
        this.server = server;
    }
    
    public void setPort(int port) {
        this.port = port;
    }

    public void setSecureSockets(boolean secureSockets) {
        this.secureSockets = secureSockets;
    }
    
    public String getServer() {
        return server;
    }

    public int getPort() {
        return port;
    }
    
    public boolean isSecureSockets() {
        return this.secureSockets; 
    }
        
    protected void debug(String message) {
        //System.out.println("DEBUG:" + message);
    }

    protected void error(String message, Exception ex) {
        System.err.println("ERROR:" + message);
        ex.printStackTrace(System.err);
    }
    
    protected void warn(String message) {
        System.err.println("WARNING:" + message);
    }

    protected Mutex createMutex(String name) {
        return new Mutex(name);  
    }
    
    /** starts the messaging-client
        @throws TimeoutException if the timeout-period elapses while waiting for a server connection
        @throws TooManyConnectionsException if the connection to the server will be immediatly closed by the server
        @throws ConnectException if the host could not be reached.
        @throws IOException if an other IOError occurs.
    */
    public void start() throws IOException {
        inputThread = new EventSink();
        outputThread =new EventSource();
        inputThread.setDaemon(true);
        outputThread.setDaemon(true);
        connect();
    }

    /** stops the messaging-client */
    public void stop() {
        if (closing || socket == null)
            return;
        try {
            debug("closing connection to server!");
            closing = true;
            /*      if (inputThread != null)
                inputThread.interrupt();
            if (outputThread != null)
                outputThread.interrupt();
            */
            if (socket!=null)
                socket.close();
            debug("Connection closed!");
        } catch (Exception ex) {
            error(ex.getMessage(),ex);
        } finally {
            socket = null;
            running = false;
            closing = false;
        }
    }

    /** @return true if the messaging client is started and connected to a servers */
    public boolean isRunning() {
        return running;
    }

    private void connect() throws IOException {
        if (isRunning())
            return;

        try{
            boolean sslConnected = false;
            if (secureSockets) {
                installProvider();
                // On some systems we need to try two times 
                try {
                    connect( createSSLFactory(), 4000);
                    sslConnected = true;
                } catch (TimeoutException ex) {
                    stop();
                    connect( createSSLFactory(), SOCKET_CONNECT_TIMEOUT);
                    sslConnected = true;
                }
            } else {
                connect( new RaplaDefaultSocketFactory(), SOCKET_CONNECT_TIMEOUT);
            }
            debug("Input Thread started.");
            inputThread.start();
            debug("Output Thread started.");
            outputThread.start();
        } catch (InterruptedException ex) {
            stop();
            throw new IOException("Connect inerrupted");
        } catch (IOException ex) {
            stop();
            throw ex;
        }
        running = true;
    }
    
    public void connect(RaplaSocketFactory socketFactory,int timeout) throws InterruptedException,IOException {
        connectLock.aquire();
        ConnectThread t = new ConnectThread( socketFactory);
        t.start(); 
        // wait for connect
        connectLock.aquire(timeout);
        connectLock.release();
        if (t.getError() != null)
            throw t.getError();
    }
    
    class ConnectThread extends Thread {
        IOException error = null;
        RaplaSocketFactory socketFactory;
        
        public ConnectThread(RaplaSocketFactory socketFactory) {
            this.socketFactory = socketFactory;
            error = null;
        }

        public IOException getError() {
            return error;
        }
        
        public void run() {
            try {
                try {
                    debug("Creating socket!");
                    socket = socketFactory.createSocket(server,port);
                    socket.setKeepAlive(true);
                    debug("Socket created!");
                } catch (IOException ex) {
                    error = ex;
                    return;
                }
                try {
                    input = new ObjectInputStream(socket.getInputStream());
                    output = new ObjectOutputStream(socket.getOutputStream());
                } catch (IOException ex) {
                    error = new TooManyConnectionsException();
                }
            } finally {
                connectLock.release();
            }
        }
    }

    private void installProvider() {
        String providerClassName = "com.sun.net.ssl.internal.ssl.Provider";
        try {
            // This is for JDK 1.3 support
            Class providerClass = getClass().getClassLoader().loadClass( providerClassName );
            Security.addProvider((Provider) providerClass.newInstance());
        } catch (SecurityException ex) {
           warn("Can't install '" + providerClassName + "'. No security permissions.");
        } catch (Throwable ex) {
            // We can ignore this, because the call below will throw an exception, if
            // the provider is needed and not installed.
        }
    }

    private RaplaSocketFactory createSSLFactory() throws IOException {
        String sslClassName = "org.rapla.components.messaging.RaplaSSLSocketFactory";
        try {
            Class sslClass = getClass().getClassLoader().loadClass( sslClassName );
            return (RaplaSocketFactory) sslClass.newInstance();
        } catch (ClassNotFoundException ex) {
            throw new IOException("Can't find '" + sslClassName + "'. Need to compile with ssl-support.");
        } catch (Throwable ex) {
            throw new IOException("Can't instanciate '" + sslClassName + "'. Need to run with ssl-support. " + ex.getMessage());
        }
    }

    
    public void addRemoteServerListener(RemoteServerListener listener) {
        listenerList.add(listener);
    }

    public void removeRemoteServerListener(RemoteServerListener listener) {
        listenerList.remove(listener);
    }

    public RemoteServerListener[] getRemoteServerListeners() {
        synchronized (listenerList) {
            return (RemoteServerListener[])listenerList.toArray(new RemoteServerListener[]{});
        }
    }

    protected void fireServerHangup() {
        RemoteServerListener[] listeners = getRemoteServerListeners();
        for (int i = 0;i<listeners.length; i++) {
            listeners[i].serverHangup();
        }
    }

    public void setCallbacks(String role,Object dispatcher) {
        callbacks.put(role,dispatcher);
    }

    public void removeCallbacks(String role) {
        callbacks.remove(role);
    }

    public int getSocketConnectTimeout() {
        return SOCKET_CONNECT_TIMEOUT;
    }

    public int getSocketAliveTimeout() {
        return SOCKET_ALIVE_TIMEOUT;
    }

    public Object call(String service,String methodName,Object[] arguments) throws
        InvocationTargetException,TimeoutException,ServiceNotFoundException,IOException 
    {
        if (!isRunning())
            throw new IllegalStateException("Connection to Server is closed!");
        try {
            callLock.aquire();
            RPCResultEvent resultEvent;
            try {
                 inputThread.newResult();
                 outputThread.processCall(new RPCCallEvent(service,methodName,arguments));
                 debug("Waiting for result of call.");
                 resultEvent = inputThread.getResult();
            } catch (TimeoutException ex) {
                throw ex;
            } catch (SocketException ex) {
                stop();
                throw ex;
            } catch (InterruptedException ex) {
                stop();
                throw new SocketException("Connection to Server closed!");
            }
            if (!isRunning()) {
                throw new SocketException("Connection to Server is closed!");
            }
            if (resultEvent.getCause() != null) {
                if (resultEvent.getCause() instanceof ServiceNotFoundException)
                    throw (ServiceNotFoundException) resultEvent.getCause();
                throw new InvocationTargetException(resultEvent.getCause());
            } else {
                return resultEvent.getResult();
            }
        } catch (InterruptedException ex) {
            throw new IOException("Call was interrupted.");
        } finally {
            callLock.release();
        }
    }

    /** processes the outgoing calls */
    class EventSource extends Thread {
        RPCCallEvent event;
        IOException error;
        Mutex eventLock = createMutex("eventLock");
        Mutex outputFinishedLock = createMutex("outputFinishedLock");

        public void processCall(RPCCallEvent event) throws IOException
                                                           ,InterruptedException
                                                           ,InvocationTargetException
        {
            outputFinishedLock.aquire();
            this.event = event;
            eventLock.release();
            try {
                error = null;
                // wait for call to be processed
                outputFinishedLock.aquire();
                if (error != null) {
                    throw error;
                }
            } finally {
                outputFinishedLock.release();
            }
        }

        public void run() {
            debug("Output thread started" + this);
            try {
                while (!isInterrupted()) {
                    while (this.event == null) {
                        eventLock.aquire();
                    }
                    output.writeObject(event);
                    output.flush();
                    // This reset is important to disable the reference sharing mechanism,
                    // which could lead to undeterministic behaviour.
                    output.reset();
                    this.event = null;
                    outputFinishedLock.release();
                }
            } catch (IOException ex) {
                error = ex;
            } catch (InterruptedException ex) {
            } finally {
                debug("Output thread terminated:" + this);
            }
        }
    }

    /** processes the incomming events*/
    class EventSink extends Thread {
        RPCResultEvent resultEvent;
        Mutex resultLock = createMutex("resultLock");
        boolean starting = false;
        Exception error;
        boolean alive = false;
        
        /** before each call we must initialize the result*/
        public void newResult() throws InterruptedException {
            alive = false;
            this.resultEvent = null;
            resultLock.aquire();
        }

        public RPCResultEvent getResult() throws InterruptedException,IOException,InvocationTargetException,ServiceNotFoundException {
            try { 
                while (resultEvent == null) {
                    // wait for the result
                    resultLock.aquire( SOCKET_ALIVE_TIMEOUT );
                    if (error != null) {
                        processError(error);
                    }
                    if (!alive && resultEvent == null) {
                        throw new TimeoutException();
                    }
                }
                if (resultEvent == null) {
                    throw new TimeoutException();
                }
                return resultEvent;
            } finally {
                resultLock.release();
            }
        }

        private void processError(Exception ex) throws IOException
                                                       ,InvocationTargetException
                                                       ,ServiceNotFoundException
        {
            if (ex instanceof InvocationTargetException) {
                throw (InvocationTargetException)ex;
            }
            if (ex instanceof IOException) {
                throw (IOException) ex;
            }
            if (ex instanceof ClassNotFoundException) {
                throw new ServiceNotFoundException("Unknown EventClass "
                                                  + ex.getMessage());
            }
            if (ex instanceof Exception) {
                throw new IOException(ex.getMessage());
            }
        }

        public void run() {
            debug("Input thread started" + this);
            try {
                while (!isInterrupted()) {
                    Object event = input.readObject();
                    dispatchEvent(event);
                }
            } catch (Exception ex) {
                error = ex;
                if (!closing && isRunning()) {
                    MessagingClientImpl.this.stop();
                    fireServerHangup();
                }
            } finally {
                resultLock.release();
                debug("Input thread terminated" + this);
            }
        }

        private void dispatchEvent(Object event) throws Exception {
            if (event instanceof RPCAliveEvent) {
                alive = true;
                return;
            }
            if (event instanceof RPCResultEvent) {
                resultEvent = (RPCResultEvent) event;
                // result is there we can release the result lock so the getResult can continue;
                resultLock.release();
            } else if (event instanceof RPCCallEvent) {
                RPCCallEvent call = (RPCCallEvent) event;
                String role = call.getRole();
                if (role == null) {
                    throw new ServiceNotFoundException("Role is null in " + call);
                }
                Object callback = callbacks.get(role);
                if (callback == null)
                    throw new ServiceNotFoundException("No callback registered for role: " + role);

                try {
                    call.dispatchEventOn(callback);
                } catch (InvocationTargetException ex) {
                    throw ex;
                } catch (Exception ex) {
                    throw new ServiceNotFoundException("Error on executing callback method" + ex.getMessage());
                }
            } else {
                throw new ServiceNotFoundException("Unknown Result type " + event
                                  + "  RPCResultEvent or RPCCallEvent expected.");
            }
        }
    }
    
	/**
	 * @return
	 */
	public boolean isLocked() {
		return callLock.locked; 
	}

	
	public void waitOnCallLock(long millis) throws InterruptedException{
		synchronized(callLock){
			callLock.wait( millis);
		}
	}

}

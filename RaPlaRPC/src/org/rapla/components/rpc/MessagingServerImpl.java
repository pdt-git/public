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
import java.io.InterruptedIOException;
import java.lang.reflect.InvocationTargetException;
import java.net.BindException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.security.Provider;
import java.security.Security;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

/** Messaging component that is used on the server side.
 * <p>Sample configuration
<pre>
   &lt;messaging-server id="messaging">
      &lt;port>8050&lt;/port>
      &lt;max-connections>myserver.mydomain&lt;/max-connections>
      &lt;secure-sockets>yes&lt;/secure-sockets>
   &lt;/messaging-server>
</pre>
 * </p>
 * You can expose new services available to the MessagingClient with:
<pre>
    server.setRemoteService("testservice",new TestRemoteService(server));
</pre>
 See the source-code of TestRemoteService for an example service.
 @see MessagingClient
*/
public class MessagingServerImpl implements MessagingServer {

    private Vector clients = new Vector();
    private ServerSocket socket = null;
    private boolean closingSocket = false;
    private int clientCount = 0;
    protected int maxConnections = 50;
    protected int port;
    protected boolean secureSockets;
    private Map serviceSelector = new HashMap();
    private boolean running;
    private ServerThread serverThread;
    
    public static final String SERVER_PORT = "port";
    public static final String MAX_CONNECTIONS = "max-connections";
    public static final String SECURE_SOCKETS = "secure-sockets";

    public void setMaxConnections(int maxConnections) {
        this.maxConnections = maxConnections;
    }

    public int getMaxConnections() {
        return this.maxConnections;
    }
    
    public void setPort(int port) {
        this.port = port;
    }

    public int getPort() {
        return port;
    }
    
    public void setSecureSockets(boolean secureSockets) {
        this.secureSockets = secureSockets;
    }
       
    public boolean isSecureSockets() {
        return this.secureSockets; 
    }
    
    public void setRemoteService(String role,RemoteService service) {
        serviceSelector.put(role,service);
    }

    public void removeRemoteService(String role) {
        serviceSelector.remove(role);
    }
    
    protected void debug(String message) {
        //System.out.println("DEBUG-rpcserver:" + message);
    }

    protected void info(String message) {
        //System.out.println("INFO-rpcserver:" + message);
    }

    protected void error(String message, Exception ex) {
        System.err.println("ERROR-rpcserver:" + message);
        ex.printStackTrace(System.err);
    }

    protected void error(String message) {
        System.err.println("ERROR-rpcserver:" + message);
    }
    
    protected void warn(String message) {
        System.err.println("WARNING:" + message);
    }

    public synchronized void start() throws IOException {
        debug("Starting Messaging-service on port " + port);
        clientCount = 0;
        closingSocket = false;
        try {
            socket = createSocketFactory().createServerSocket(port,10);
        } catch (BindException ex) {
            throw new BindException("Could not bind to port " + port + ". Typically, the port is already used: "  + ex.getMessage());
        }
        try {
            getClass().getClassLoader().loadClass("java.net.SocketTimeoutException");
        } catch (ClassNotFoundException ex) {
            // We have to set a socket timeout for JDK<1.3
            socket.setSoTimeout(2000);
        }
        serverThread = new ServerThread();
        serverThread.setDaemon(false);
        serverThread.start();
        info("Messaging-service started on port " + port + " and awaiting Connetions ");
        running = true;
    }

    public synchronized void stop() throws IOException {
        if ( ! running ) {
            warn("Service is not running ");
            return;
        }
        try {
            synchronized (serverThread) {
                if (socket != null) {
                    info("Stopping MessagingService ");
                    closingSocket = true;
                    ClientConnectionImpl[] array = (ClientConnectionImpl[])clients.toArray(new ClientConnectionImpl[0]);
                    for (int i=0;i<array.length;i++) {
                        array[i].close();
                    }
                    socket.close();
                }
            }
            int tries = 0;
            while ( !serverThread.clientsClosed() && tries < 10 ) {
                Thread.sleep(20);
                tries ++;
            }
            if ( tries == 10) {
                warn("Can't close clients properly");
            }
        } catch (InterruptedException ex) {
            error("MessagingService was not cleanly disconnected.");
        } finally {
            running = false;
            info("Messaging service stopped");
        }
    }

    public boolean isRunning() {
        return running;
    }

    private RaplaSocketFactory createSocketFactory() {
        RaplaSocketFactory socketFactory = null;
        boolean sslSupport = false;
        if (secureSockets) {
            String sslClassName = "org.rapla.components.messaging.RaplaSSLSocketFactory";
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
            try {
                Class sslClass = getClass().getClassLoader().loadClass( sslClassName );
                return (RaplaSocketFactory) sslClass.newInstance();
            } catch (ClassNotFoundException ex) {
                warn("Can't find '" + sslClassName + "'. Need to compile with ssl-support.");
            } catch (Throwable ex) {
                warn("Can't instanciate '" + sslClassName + "'. Need to run with ssl-support.");
            }
        }
        if (secureSockets)
            warn("SSLSupport disabled.");
        return new RaplaDefaultSocketFactory();
    }

    class ServerThread extends Thread {
    	public ServerThread() {
    		super("Server Accept Loop");
		}
        IOException error;
        boolean clientsClosed = false;
        public IOException getError() {
            return error;
        }
        
        public boolean clientsClosed() {
            return clientsClosed;
        }

        public void run() {
            try {
                while ( true ) {
                    try {
                        synchronized( this ) {
                            if ( closingSocket )
                                return;
                        }
                        Socket connection = socket.accept();
                        connection.setKeepAlive(true);
                        if (clientCount >= maxConnections) {
                            warn("Maxmimum number of connections [" + maxConnections + "] reached dropping " + connection.getInetAddress() + ":" + connection.getPort());
                            connection.close();
                            continue;
                        }
                        ClientConnectionImpl clientThread = new ClientConnectionImpl(connection
                                                                                     ,MessagingServerImpl.this);
                        info("new client " + clientThread.getClientName());
                        clientThread.start();
                    } catch (InterruptedIOException ex) {
                        continue;
                    } catch (SocketException ex) {
                        if (closingSocket) {
                            info("Server disconnected");
                            return;
                        } else {
                            error(ex.getMessage());
                            continue;
                        }
                    }
                }
            } catch (IOException ex) {
                error(ex.getMessage(),ex);
                try {
                    MessagingServerImpl.this.stop();
                } catch (IOException e) {
                    error(e.getMessage());
                }
            } finally {
                clientsClosed =true;
            }
        }
    }

    synchronized void subscribe(ClientConnectionImpl client) {
        info("Client subscribed " + client.getClientName());
        clients.add(client);
        clientCount++;
        debug("Subscribed Clients " + clientCount);
    }

    synchronized void unsubscribe(ClientConnectionImpl client) {
        if (clients.remove(client)) {
            info("Client unsubscribed " + client.getClientName());
            clientCount --;
            debug("Subscribed Clients " + clientCount);
        }
    }

    void dispatch(ClientConnectionImpl client,RPCCallEvent call) throws IOException {
        boolean notify = true;
        try {
            // first notify the client that the event reached the server;
            client.writeEvent( new RPCAliveEvent() );
            RPCResultEvent resultEvent;
            try {
                RemoteService dispatcher = (RemoteService)
                    serviceSelector.get(call.getRole());
                if (dispatcher == null)
                    throw new ServiceNotFoundException("RemoteService for Role '" + call.getRole()
                                               + "' not found in EventServer. Call setRemoteService().");
                Object result = dispatcher.dispatch(client,call);
                resultEvent = new RPCResultEvent(result);
            } catch (InvocationTargetException ex) {
                resultEvent = new RPCResultEvent();
                resultEvent.setCause(ex.getTargetException());
            } catch (Exception ex) {
                resultEvent = new RPCResultEvent();
                resultEvent.setCause(ex);
            }
            client.writeEvent(resultEvent);
        } catch (QueueFullException ex) {
            client.close();
        }
    }

    final public void callback(ClientConnection client,RPCCallEvent call) throws IOException {
        try {
            debug("Callback client " + client.getClientName());
            ((ClientConnectionImpl)client).writeEvent(call);
        } catch (QueueFullException ex) {
            error("Client-Queue full. Closing client: " + client);
            ((ClientConnectionImpl)client).close();
            throw new IOException("Client-Queue full. Client closed");
        }
    }
	/**
	 * @return Returns the clientCount.
	 */
	public int getClientCount() {
		return clientCount;
	}
}



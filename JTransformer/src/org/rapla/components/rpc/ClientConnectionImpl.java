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
import java.io.*;
import java.net.*;
import java.util.HashMap;
import java.util.ArrayList;
import java.util.EventObject;

import org.cs3.pl.prolog.PrologClientConnection;



class ClientConnectionImpl extends Thread implements PrologClientConnection {
    Socket socket;
    MessagingServerImpl server;
    ObjectInputStream input;
    ObjectOutputStream output;
    boolean closed = false;
    public static int MAX_EVENTS_IN_QUEUE = 50;
    Queue queue = new Queue(MAX_EVENTS_IN_QUEUE);
    OutputThread outputThread;
    HashMap map = new HashMap();
    String socketName;
    ArrayList listeners = new ArrayList();
	private boolean userInteraction = false;

    public ClientConnectionImpl(Socket socket,MessagingServerImpl server) {
    	this.socket = socket;
    	this.server = server;
    	socketName = socket.getInetAddress() + ":" + socket.getPort();
    	this.setDaemon(true);
    } 

    public void putSessionValue(String key,Object value) {
        map.put(key,value);
    }

    public Object getSessionValue(String key) {
        return map.get(key);
    }

    public void run() {
    	try {
            //  Test Code: Setting Buffer-Size to a minimum to test
            //  proper Exception-Handling
            // 
    	    //	    s.setSendBufferSize(100);
    	    //	    s.setReceiveBufferSize(100);
    	    //	    System.out.println("<" + s.getSendBufferSize() + " >" + s.getReceiveBufferSize());
    	    output = new ObjectOutputStream(socket.getOutputStream());
    	    input = new ObjectInputStream(socket.getInputStream());
    	    outputThread = new OutputThread();
    	    outputThread.setDaemon(true);
    	    server.subscribe(this);
    	} catch (IOException ex) {
    	    close();
    	    return;
    	}
    	if (closed)
    	    return;
    	outputThread.start();
    	while(!closed) {
    	    try {
    	        RPCCallEvent event = (RPCCallEvent) input.readObject();
    	        server.dispatch(this,event);
    	    } catch (ClassNotFoundException ex) {
                server.error( this.getClientName() + " No valid method call. Unknown EventClass. Disconnecting client.");
        		close();
    	    } catch (ClassCastException ex) {
                server.error( this.getClientName() + " No valid method call. Disconnecting client.");
        		close();
    	    } catch (EOFException ex) {
        		close();
        		return;
    	    } catch (InterruptedIOException ex) {
        		close();
        		return;
    	    } catch (SocketException ex) {
        		close();
        		return;
    	    } catch (IOException ex) {
        	    server.error(getClientName() + ex.getMessage(),ex);
        		close();
        		return;
    	    }
    	}
    }

    public InetAddress getClientAddress() {
    	if (socket != null)
    	    return socket.getInetAddress();
    	return null;
    }
    public void close() {
    	if (closed)
    	    return;
    	server.debug("Closing connection to " + getClientName());
    	try {
    	    socket.close();
    	    if (input != null)
    	        input.close();
    	    if (output != null)
    	        output.close();
    	} catch (IOException ex) {
    	} finally {
    	    closed = true;
    	    server.unsubscribe(this);
    	    fireConnectionClosed();
    	}
    }

    private Object getNextEvent() throws InterruptedException {
    	while (!closed) {
    	    if (queue.isEmpty()) {
        		synchronized(queue) {
        		    queue.wait();
        		}
    	    } else {
    	        return queue.dequeue();
    	    }
    	}
    	return null;
    }

    public void writeEvent(Object event) throws IOException,QueueFullException {
    	queue.enqueue(event);
        synchronized (queue) {
            queue.notifyAll();
        }
    }

    class OutputThread extends Thread {
        public void run() {
            try {
            	while(!closed) {
            	    Object event = getNextEvent();
            	    if (!closed) {
            	        output.writeObject(event);
            	        output.flush();
            	        //This reset is important to disable the reference sharing mechanism,
            	        //  which could lead to undeterministic behaviour.
            	        output.reset();
                    }
                }
                server.debug("Output thread stopped " + getClientName());
    	    } catch (Exception ex) {
    	    } finally {
    	        close();
    	    }
    	}
    }

    public ConnectionListener[] getConnectionListeners() {
		synchronized(listeners) {
		    return (ConnectionListener[])listeners.toArray(ConnectionListener.ARRAY);
		}
	}

    public void addConnectionListener(ConnectionListener listener) {
        listeners.add(listener);
    }

    public void removeConnectionListener(ConnectionListener listener) {
        listeners.add(listener);
    }

    public void fireConnectionClosed() {
		ConnectionListener[] array = getConnectionListeners();
		EventObject event = null;
		for (int i=0;i<array.length;i++) {
		    if (event == null)
			event = new EventObject(this);
		    array[i].connectionClosed(event);
		}
    }

    public void dispose() {
        listeners.clear();
    }

    public String getClientName() {
        return socketName;
    }

	public boolean hasUserInteraction() {
		return userInteraction;
	}
	
	public boolean setUserInteraction(boolean userInterfaction) {
		return this.userInteraction = userInteraction;
	}

}

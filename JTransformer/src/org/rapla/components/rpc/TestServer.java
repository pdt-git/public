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
import java.util.*;
import java.io.IOException;

/** A sample server providing two methods store and fail to the client.
    You can start the server with the main-method.
 */
public class TestServer implements RemoteService,ConnectionListener {
    int id =0;
    MessagingServerImpl server;
    Set clients = new HashSet();
    public static String ROLE = TestServer.class.getName();
    
    public TestServer() throws Exception {
        this.server = new MessagingServerImpl();
        server.setPort(5000);
        server.setRemoteService(ROLE, this);
        server.start();
    }
    
    public Object dispatch(ClientConnection client,RPCCallEvent call) throws Exception {
    	// add the client to list of registered services
        print(" client registered " + client.getClientName());
        synchronized ( clients ) {
            clients.add(client);
        }
    	client.addConnectionListener(this);
    	return call.dispatchEventOn(this);
    }

    public void print(String message) {
        System.out.println( message );
    }
    
    public String store(String string) {
    	String newString = string + "[" + (id++) +"]";
    	RPCCallEvent updateEvent = new RPCCallEvent(TestClient.ROLE
    						    ,"fireUpdate"
    						    ,new Object[] {newString});
    	print(newString + " value stored notifying clients.");
        synchronized ( clients ) {
        	Iterator it = clients.iterator();
        	while (it.hasNext()) {
        	    try {
        		server.callback((ClientConnection)it.next()
        				,updateEvent);
        	    } catch (IOException ex) {
        		ex.printStackTrace();
        	    }
        	}
        } 
    	return newString;
    }
    
    public void doSomethingLong() {
        try {
            Thread.sleep(2000);
        } catch (InterruptedException ex) {
        }
    }

    public void fail() {
        int i = 1/0;
    }

    public void connectionClosed(EventObject obj) {
    	ClientConnection client = (ClientConnection) obj.getSource();
        synchronized ( clients ) {
            clients.remove(client);
        }
    	print(" client unregistered " + client.getClientName());
    }

    public static void main(String[] args) {
    	try {
            new TestServer();
    	} catch (Exception ex) {
    	    ex.printStackTrace();
    	}
    }

}


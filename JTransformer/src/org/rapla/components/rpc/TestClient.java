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
import java.lang.reflect.InvocationTargetException;

/** A sample client providing the callback method fireUpdate.
    You can start the client with the main-method.
 */
public class TestClient {
    public static String ROLE= TestClient.class.getName();
    MessagingClientImpl remote = new MessagingClientImpl();

    public void start() throws Exception {
    	remote.setServer("localhost");
        remote.setPort(5000);
        remote.setCallbacks(ROLE,this);
        remote.start();
    }
    
    public void stop() throws Exception {
        remote.stop();
    }
    
    public String store() throws Exception {
    	long time =System.currentTimeMillis();
    	try {
    	    Object result = remote.call(TestServer.ROLE
    					,"store"
    					,new Object[] {String.valueOf(time)});
    	    print(result + " stored");
            return result.toString();
    	} catch (InvocationTargetException ex) {
    	    throw (Exception) ex.getTargetException();
        }
    }
    
    public void doSomethingLong() throws Exception {
        try {
            remote.call(TestServer.ROLE
                ,"doSomethingLong",new Object[] {});
        } catch (InvocationTargetException ex) {
            throw (Exception) ex.getTargetException();
        }
    }
        
    public void fail() throws IOException {
    	try {
    	    remote.call("testservice","fail",new Object[] {});
    	} catch (InvocationTargetException ex) {
    	    ex.getTargetException().printStackTrace();
    	} catch (Exception ex) {
    	    ex.printStackTrace();
    	}
    }

    /** This callback method is called by the TestRemoteService
	@see TestServer
     */
    public void fireUpdate(String string) {
        print("fireUpdate called on client with: " + string);
    }

    public void print(String message) {
        System.out.println( message );
    }
    public static void main(String[] args) {
    	try {
    	    TestClient client = new TestClient();
    	    client.start();
    	    client.store();
    	    System.out.println("Waiting one minute for incomming messages");
    	    Thread.sleep(60000);
            client.stop();
    	    System.exit(0);
    	    //	    client.fail();
    	} catch (Exception ex) {
    	    ex.printStackTrace();
    	    System.exit(1);
    	}
    }

}


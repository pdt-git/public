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
import java.net.ServerSocket;
import java.net.Socket;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

public class RaplaSSLSocketFactory implements RaplaSocketFactory {
    public Socket createSocket(String host, int port) throws IOException
    {
    	SSLSocket socket =(SSLSocket) SSLSocketFactory.getDefault().createSocket(host, port);
    	String ciphers[] = socket.getSupportedCipherSuites();
    	socket.setEnabledCipherSuites(ciphers);
    	//socket.setEnabledCipherSuites(new String[] {ciphers[0]}); 
    	return socket;
    }
    
    public ServerSocket createServerSocket(int port,int maxConnections)
    	throws IOException
    {
    	SSLServerSocket socket = (SSLServerSocket) 
    	    SSLServerSocketFactory.getDefault().createServerSocket(port
    								   ,maxConnections);
    	String ciphers[] = socket.getSupportedCipherSuites();
    	socket.setEnabledCipherSuites(ciphers);
    	//	socket.setEnabledCipherSuites(new String[] {ciphers[0],ciphers[1]}); 
    	return socket;
    }
}
	

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

public class RaplaDefaultSocketFactory implements RaplaSocketFactory {
    public Socket createSocket(String host, int port)
	throws IOException
    {
	return new Socket(host,port);
    }
    
    public ServerSocket createServerSocket(int port,int maxConnections) 
	throws IOException
    {
	return new ServerSocket(port,maxConnections);
    }
}
	

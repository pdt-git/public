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

import org.cs3.pl.common.Debug;






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
public class MessagingServerLoggingWrapper extends MessagingServerImpl
{
    private Logger logger;

    public void configure(int maxConnections, int port,boolean secureSockets) {
    	Debug.info("Sever configured: max. Connections=" +maxConnections
    			+", port="+port+", secureSockets="+secureSockets );		
    	this.maxConnections = maxConnections;
    	this.port = port;
    	this.secureSockets =secureSockets; 
//        this.secureSockets = config.getChild("secure-sockets").getValueAsBoolean(false);
    }
    
    
    public void enableLogging(Logger logger) {
        this.logger = logger;
    }

    protected Logger getLogger() {
        return logger;
    }
    
    protected void debug(String message) {
        getLogger().debug(message);
        Debug.debug(message);
    }

    protected void info(String message) {
        getLogger().info(message);
        Debug.info(message);
    }

    protected void error(String message, Exception ex) {
        getLogger().error(message,ex);
        Debug.error(message);
        Debug.report(ex);
    }

    protected void error(String message) {
        getLogger().error(message);
        Debug.error(message);
    }
    
    protected void warn(String message) {
        getLogger().warn(message);
        Debug.warning(message);
    }
}

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


//import org.apache.avalon.framework.context.*;
//import org.apache.avalon.framework.configuration.*;
//
//import org.apache.avalon.framework.logger.Logger;
//import org.apache.avalon.framework.logger.LogEnabled;

/** Messaging component that is used on the client side.
 * <p>Sample configuration 1:
<pre>
   &lt;messaging-client id="default-server">
      &lt;server>myserver.mydomain&lt;/server>
      &lt;port>8050&lt;/port>
      &lt;secure-sockets>yes&lt;/secure-sockets>
   &lt;/messaging-client>
</pre>

   The port and server value specify the address of the
   MessagingClient. If secure-sockets is set an ssl-connection will be
   used for communicating with the server. This requires a server
   that provides an ssl-connection on the specified port
   {@link RaplaSSLSocketFactory}
   </p>
   <p>Sample configuration 2:
<pre>
   &lt;messaging-client id="second-server">
      &lt;server>${download-server}&lt;/server>
      &lt;port>8079&lt;/port>
   &lt;/messaging-client>
</pre>
  Note that the host is dynamicaly retrieved from the components
  context "download-server" entry.  SSL is disabled in this
  configuration.  {@link Contextualizable}
</p>
 See the source-code of TestRemoteClient for an example client.
  @see MessagingServer
*/
public class MessagingClientLoggingWrapper extends MessagingClientImpl
{
    private Logger logger;
    
    public void configure(String server, int port, boolean secureSockets) {
    	this.server = server;
    	this.port = port;
    	this.secureSockets = secureSockets;
    }
    
    public void configure(String server, int port) {
    	configure(server,port,false);
    }

    public void enableLogging(Logger logger) {
        this.logger = logger;
    }

    protected Logger getLogger() {
        return logger;
    }
    
    
    protected void debug(String message) {
        getLogger().debug(message);
    }

    protected void info(String message) {
        getLogger().info(message);
    }

    protected void error(String message, Exception ex) {
        getLogger().error(message,ex);
    }

    protected void error(String message) {
        getLogger().error(message);
    }
    
    protected void warn(String message) {
        getLogger().warn(message);
    }
    protected Mutex createMutex(String name) {
        return new LoggerMutex(logger, name);
    }

}

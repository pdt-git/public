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

/** Messaging component that is used on the server side.
 * You can expose new services available to the MessagingClient with:
<pre>
    server.setRemoteService("testservice",new TestRemoteService(server));
</pre>
 See MessagingServerImpl for the configuration options of
 the default Implementation.

 See the source-code of TestRemoteService for an example service.
 @see MessagingServerImpl
 @see MessagingClient
*/
public interface MessagingServer {
    public static final String ROLE = MessagingServer.class.getName();

    public void setRemoteService(String role,RemoteService service);
    public void removeRemoteService(String role);
    public boolean isRunning();

    public void callback(ClientConnection client,RPCCallEvent call) throws IOException;
}



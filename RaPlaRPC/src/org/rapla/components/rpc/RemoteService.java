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

public interface RemoteService {
    /** Dispatches a call from the client to a service on the server.
     * This method is called, when a the Messaging server receives a call from a connected client to a 
     * specified service on the server. The MessagingServer does a lookup the specified service in its service-list
     * and calls the dispatch method on this service. The service can
     * then call the selected method with <code>call.dispatchEventOn</code>
     */
    public Object dispatch(ClientConnection client,RPCCallEvent call) throws Exception;
}

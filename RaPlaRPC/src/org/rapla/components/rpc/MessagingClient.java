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

/** Messaging component that is used on the client side.
    See MessagingClientImpl for the configuration options of
    the default Implementation.
    See the source-code of TestRemoteClient for an example client.
  @see MessagingClientImpl
  @see MessagingServer
*/
public interface MessagingClient {
    public static final String ROLE = MessagingClient.class.getName();

    /** starts the messaging-client
        @throws TimeoutException if the timeout-period elapses while waiting for a server connection
        @throws TooManyConnectionsException if the connection to the server will be immediatly closed by the server
        @throws ConnectException if the host could not be reached.
        @throws IOException if an other IOError occurs.
    */
    public void start() throws IOException;
    /** stops the messaging-client */
    public void stop();
    /** @return true if the messaging client is started and connected to a servers */
    public boolean isRunning();

    public void addRemoteServerListener(RemoteServerListener listener);
    public void removeRemoteServerListener(RemoteServerListener listener);
    public RemoteServerListener[] getRemoteServerListeners();
    /** @param role the role-name of the callback service
        @param dispatcher the object implementing the callback service
        Callback services allow the server to invoke methods on the client.
     */
    public void setCallbacks(String role,Object dispatcher);

    /** Removes callback service
        @see #setCallbacks
     */
    public void removeCallbacks(String role);

    /** the host-name of the computer running the MessagingServer
        @see MessagingServer
     */
    public String getServer();

    /** the port of the the MessagingServer
        @see MessagingServer
     */
    public int getPort();

    /** calls a method on the specified server-service
        @param service role-name of the server-service
        @param methodName name of the service method
        @throws TimeoutException if no response from server after a specified timeout-period
     */
    public Object call(String service,String methodName,Object[] arguments) throws
        InvocationTargetException
        ,TimeoutException
        ,ServiceNotFoundException
        ,IOException;
}

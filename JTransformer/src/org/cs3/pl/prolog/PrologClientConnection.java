/*
 * Created on 10.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.prolog;

import org.rapla.components.rpc.ClientConnection;

/**
 * @author xproot
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public interface PrologClientConnection extends ClientConnection {
	
	/**
	 * Returns true if a Console is attached to the Client
	 * and a interaction is possible.
	 * In this case the exception handling is done in the Prolog
	 * process. Otherwise the exception is wrapped into a Java PrologException.
	 * 
	 * @return
	 */
	
	public boolean hasUserInteraction();

	public boolean setUserInteraction(boolean userInterfaction);

}

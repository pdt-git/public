/*
 * Created on 21.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.prolog;

import java.util.EventListener;


/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public interface IPrologListener extends EventListener {
    /**
     * a prolog client is entering the method PrologClient.catchedCall()
     * use PrologEvent.getClient() to get a reference on the client. 
     */
    public void enterCatchedCall(PrologEvent e);
    
    /**
     * a prolog client is leaving the method PrologClient.catchedCall()
     * use PrologEvent.getClient() to get a reference on the client. 
     */    
    public void exitCatchedCall(PrologEvent e);
	
    /**
     * new output is available.
     * use PrologEvent.getStream() to find out on which stream
     * use PrologEvent.getData() to get the new data
     */
    public void newDataAvailable(PrologEvent e);
	
}

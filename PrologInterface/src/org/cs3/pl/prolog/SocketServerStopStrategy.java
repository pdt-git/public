/*
 */
package org.cs3.pl.prolog;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.socket.SocketClient;

/**
 */
public class SocketServerStopStrategy implements ServerStopStrategy {

   

    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.internal.ServerStopStrategy#stopServer(int, boolean)
     */
    public void stopServer(int port, boolean now) {
        try {
		    if(!Util.probePort(port)){
		        Debug.info("There is no server running, afaics. So i wont stop anything.");
		        return;
		    }
			SocketClient c = new SocketClient("localhost",port);		
			try{
			    c.readUntil(SocketClient.GIVE_COMMAND);
			    c.writeln(SocketClient.SHUTDOWN);		
			    c.readUntil(SocketClient.BYE);
			    c.close();
			}
			catch (Exception e) {
				Debug.warning("There was a problem during server shutdown.");
				Debug.report(e);
				if(! now){
					throw new RuntimeException(e);
				}
			}
			
			while ((Util.probePort(port))) {
				try {
					Thread.sleep(50);
				} catch (InterruptedException e1) {
					Debug.report(e1);
				}
			}
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		} 

    }

}

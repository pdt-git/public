package org.cs3.pl.prolog;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.ReusableClient;
import org.rapla.components.rpc.Logger;

public class DefaultServerStopStrategy implements ServerStopStrategy {

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ServerStopStrategy#shutDownServer(int)
	 */
	public void stopServer(int port) {
		
		try {
		    if(!Util.probePort(port)){
		        Debug.info("There is no server running, afaics. So i wont stop anything.");
		        return;
		    }
			ReusableClient s = new ReusableClient();
			s.enableLogging(new Logger("default"));
			s.configure("localhost", port);
			s.start();						
			s.call("RemotePrologSession","shutdownServer",new Object[0]);
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

package org.cs3.pl.prolog;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.rpc.ReusableRPCClient;
import org.rapla.components.rpc.Logger;

public class RPCServerStopStrategy implements ServerStopStrategy {

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ServerStopStrategy#shutDownServer(int)
	 */
	public void stopServer(int port, boolean now) {
		
		try {
		    if(!Util.probePort(port)){
		        Debug.info("There is no server running, afaics. So i wont stop anything.");
		        return;
		    }
			ReusableRPCClient s = new ReusableRPCClient();
			s.enableLogging(new Logger("default"));
			s.configure("localhost", port);
			s.start();		
			try{
				s.call ("RemotePrologSession","shutdownServer",new Object[]{new Boolean(now)});
				s.stop();
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

package org.cs3.pl.prolog;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.ReusableClient;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.ServiceNotFoundException;
import org.rapla.components.rpc.TimeoutException;

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
		} catch (TimeoutException e) {
			Debug.report(e);
		} catch (InvocationTargetException e) {
			Debug.report(e);
		} catch (ServiceNotFoundException e) {
			Debug.report(e);
		} catch (IOException e) {
			Debug.report(e);
		}
	}

}

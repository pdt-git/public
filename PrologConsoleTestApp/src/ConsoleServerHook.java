import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;

/**
 * An Init/ShutdownHook that takes the neccesary steps to open/close
 * an io socket on the server process to which a 
 * Console can be connected.
 */
public class ConsoleServerHook implements LifeCycleHook{
	
	
	public void onInit(PrologSession s) {
		if (Util.probePort(5567, "end_of_file.\n")) {
			Debug
					.info("Console server thread seems to be running, so i will not start a new one.");			
		}else{
			try {
				s
						.query("use_module(library(prolog_server)), prolog_server(5567, [])");
			} catch (SessionException e) {
				Debug.report(e);
			}
			Debug.debug("Server thread created");
		}
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ShutdownHook#beforeShutDown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologSession session) {
		
		if (!Util.probePort(5567, "end_of_file.\n")) {
			Debug
					.info("Console server thread does not seem to be running, so i will not stop it.");
			return;
		}
		try {
			session.query("thread_signal(prolog_server,thread_exit(0))");
		} catch (SessionException e) {
			Debug.report(e);
		}
		Debug.debug("Server thread stopped");
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit() {

		
	}

	

	

}
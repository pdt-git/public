import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.InitHook;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;
import org.cs3.pl.prolog.ShutdownHook;

/**
 * An Init/ShutdownHook that takes the neccesary steps to open/close
 * an io socket on the server process to which a 
 * Console can be connected.
 */
public class ConsoleServerHook implements InitHook,ShutdownHook {
	private Vector initHooks = new Vector();
	private Vector shutdownHooks = new Vector();

	public void addSubInitHook(InitHook h){
		synchronized(initHooks){
			if(! initHooks.contains(h)){
				initHooks.add(h);
			}
		}
	}
	
	public void addSubShutdownHook(ShutdownHook h){
		synchronized(shutdownHooks){
			if(! shutdownHooks.contains(h)){
				shutdownHooks.add(h);
			}
		}
	}
	
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
		synchronized(initHooks){
			for (Iterator it = initHooks.iterator(); it.hasNext();) {
				InitHook h = (InitHook) it.next();
				h.onInit(s);
			}
		}
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ShutdownHook#beforeShutDown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutDown(PrologSession session) {
		synchronized(shutdownHooks){
			for (Iterator it = shutdownHooks.iterator(); it.hasNext();) {
				ShutdownHook h = (ShutdownHook) it.next();
				h.beforeShutDown(session);
			}
		}
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

	

}
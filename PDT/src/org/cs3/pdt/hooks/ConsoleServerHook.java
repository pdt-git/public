package org.cs3.pdt.hooks;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;

/**
 * An Init/ShutdownHook that takes the neccesary steps to open/close
 * an io socket on the server process to which a 
 * Console can be connected.
 */
public class ConsoleServerHook implements LifeCycleHook{
	
	
	public static final String HOOK_ID = "org.cs3.pdt.hooks.ConsoleServerHook";

	 private static int getPort() {
	        IPreferencesService service = Platform.getPreferencesService();
	        String qualifier = PDTPlugin.getDefault().getBundle().getSymbolicName();
	        int port= service.getInt(qualifier,Properties.CONSOLE_PORT,-1,null);       
	    	if(port==-1){
	    		throw new NullPointerException("Required property \""+Properties.CONSOLE_PORT+"\" was not specified.");
	    	}
			return port;
		}
	
    public void onInit(PrologSession s) {
        int port = getPort();
		if (Util.probePort(port, "end_of_file.\n")) {
			Debug.info("Console server thread seems to be running, so i will not start a new one.");			
		}else{
		    String queryString = "use_module(library(prolog_server)), prolog_server("+port+", [])";
		    Debug.info("starting console server using: "+queryString);
		    try {				
                s.query(queryString);
			} catch (SessionException e) {
				Debug.report(e);
			}
			while(!Util.probePort(port,"end_of_file.\n")){
	             try {
	                 Thread.sleep(50);
	             } catch (InterruptedException e1) {
	                 Debug.report(e1);
	             }
	         }
			Debug.debug("Server thread created");
		}
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ShutdownHook#beforeShutDown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologSession session) {
	    /*ld: XXX FIXME TODO ARRRRGH!
	     * things do not work this way in the linux implementation of swi /clib.
	     * i have written a mail on the swi list an i am currently waiting for feedback,
	     * for now, we simply ignore the problem when on non-windows system.
	     */
//	    boolean isWindoof = System.getProperty("os.name").indexOf("Windows")>-1;
//	    if(!isWindoof){
//	        return;
//	    }
//	    
//	    int port = getPort();
//		if (!Util.probePort(port, "end_of_file.\n")) {
//			Debug
//					.info("Console server thread does not seem to be running, so i will not stop it.");
//			return;
//		}
//		String queryString = "thread_signal(prolog_server,throw(FrissStahlExcpeption))";
//	    Debug.info("stopping console server using: "+queryString);
//		try {
//			session.query(queryString);
//		} catch (SessionException e) {
//			Debug.report(e);
//		}
//		while(Util.probePort(port,"end_of_file.\n")){
//            try {
//                Thread.sleep(50);
//            } catch (InterruptedException e1) {
//                Debug.report(e1);
//            }
//        }		
//		Debug.debug("Server thread stopped");
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit() {

		
	}

	

	

}
package org.cs3.pdt.console.internal.hooks;

import java.io.File;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
//TODO: this hook should be merged into PrologConsoleView 
/**
 * An Init/ShutdownHook that takes the neccesary steps to open/close an io
 * socket on the server process to which a Console can be connected.
 */
public class ConsoleServerHook implements LifeCycleHook {

    public static final String HOOK_ID = "org.cs3.pdt.console.internal.hooks.ConsoleServerHook";
    
    private int port;
    private File lockFile;
//    private static int getPort() {
//       String value = PrologConsolePlugin.getDefault().getPreferenceValue(PDTConsole.PREF_CONSOLE_PORT, null);
//       if (value==null) {
//           throw new NullPointerException("Required property \""
//                   + PDTConsole.PREF_CONSOLE_PORT + "\" was not specified.");
//       }
//       int port = Integer.parseInt(value);        
//        return port;
//    }

    public void onInit(PrologInterface pif,PrologSession s) {
        try {
            port = Util.findFreePort();
            
            lockFile=Util.getLockFile();       
                String prologFileName = Util.prologFileName(lockFile);
				String queryString = "use_module(library(prolog_server)), prolog_server("
                        + port + ", []),assert(pdt_console_server("+port+",'"+prologFileName+"'))," +
                        		"consult_server:create_lock_file('"+prologFileName+"')";
                Debug.info("starting console server using: " + queryString);

                s.queryOnce(queryString);

                while (!lockFile.exists()) {
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException e1) {
                        Debug.report(e1);
                    }
                }
                Debug.debug("Server thread created");
            
        } catch (Throwable e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.ShutdownHook#beforeShutDown(org.cs3.pl.prolog.PrologSession)
     */
    public void beforeShutdown(PrologInterface pif,PrologSession session) {
        /*
         * ld: XXX FIXME TODO ARRRRGH! things do not work this way in the linux
         * implementation of swi /clib. i have written a mail on the swi list an
         * i am currently waiting for feedback, for now, we simply ignore the
         * problem when on non-windows system.
         */
        //	    boolean isWindoof =
        // System.getProperty("os.name").indexOf("Windows")>-1;
        //	    if(!isWindoof){
        //	        return;
        //	    }
        //	    
        //	    int port = getPort();
        //		if (!Util.probePort(port, "end_of_file.\n")) {
        //			Debug
        //					.info("Console server thread does not seem to be running, so i will
        // not stop it.");
        //			return;
        //		}
        //		String queryString =
        // "thread_signal(prolog_server,throw(FrissStahlExcpeption))";
        //	    Debug.info("stopping console server using: "+queryString);
        //		try {
        //			session.query(queryString);
        //		} catch (PrologException e) {
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

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
     */
    public void afterInit(PrologInterface pif) {

    }

}
package org.cs3.pl.prolog;

import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;



/**
 * The default way to start a PrologInterfaceServer.
 * 
 * @author degenerl
 * 
 * This implementation tries to get a reasonable classpath from the classloader.
 * This will not work when running as an eclipse plugin. But it is usefull for
 * testing.
 */
public class DefaultServerStartStrategy implements ServerStartStrategy {

    public Process startServer(final int port) {
        String classpath = System.getProperty("java.class.path");
        String librarypath = System.getProperty("java.library.path");        


        String cmdline = "java -Djava.library.path=\"" + librarypath
                + "\""/* -D"
				+Properties.PREF_DEBUG_LEVEL+"="
				+System.getProperty(Properties.PREF_DEBUG_LEVEL)*/
				+" -classpath "
                + classpath 
				+ " org.cs3.pl.prolog.PrologInterfaceServer "
                + port;
        Debug.debug("Starting server with " + cmdline);
        
        try {
  
            Process serverProcess = Runtime.getRuntime().exec(cmdline);
            while(!Util.probePort(port)){
                try {
                    Thread.sleep(50);
                } catch (InterruptedException e1) {
                    Debug.report(e1);
                }
            }

            return serverProcess;
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }
    
}
package org.cs3.pdt.internal;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.pdt.PDT;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ServerStartStrategy;

public class PDTServerStartStrategy implements ServerStartStrategy {

		
    public class _InputStreamPump extends InputStreamPump {

        public _InputStreamPump(InputStream s) {
            super(s);        
        }
        protected void dataAvailable(char[] buffer, int length) {
            Debug.debug("SERVER says: "+ new String(buffer,0,length));
        }

    }
	private String swiHome;
	private String classPath;
	private String debugLevel;

	/**
	 * @param swiHome
	 * @param classPath
	 */
	public PDTServerStartStrategy(String swiHome, String classPath, String debugLevel) {
		super();
		this.swiHome = swiHome;
		this.classPath = classPath;
		this.debugLevel = debugLevel;
	}
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ServerStartStrategy#startServer()
	 */
	public Process startServer(int port) {
		boolean isWindoof = System.getProperty("os.name").indexOf("Windows")>-1;
		String dir = isWindoof ? swiHome+"\\bin" 
					: ".";			
		
		String cmdline = "java -D"+PDT.PREF_DEBUG_LEVEL+"="
				+debugLevel
				+" -classpath "+classPath
				+ " org.cs3.pl.prolog.internal.PrologInterfaceServer " + port;
				
		Debug.debug("Starting server with " + cmdline);
		Debug.debug("dir="+dir);
		Process process =null;
		try {
			process= Runtime.getRuntime().exec(cmdline, null, new File(dir));
			new _InputStreamPump(process.getErrorStream()).start();
            new _InputStreamPump(process.getInputStream()).start();
            
            
		} catch (IOException e1) {		
			Debug.report(e1);
			return null;
		}
		 
		while(!Util.probePort(port)){
             try {
                 Thread.sleep(50);
             } catch (InterruptedException e1) {
                 Debug.report(e1);
             }
         }
		return process;
	}

}

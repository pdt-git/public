package org.cs3.pl.prolog.internal.rpc;

import java.io.File;
import java.io.InputStream;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.InputStreamPump;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.internal.ServerStartStrategy;

public class AnotherRPCServerStartStrategy implements ServerStartStrategy {

		
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
	public AnotherRPCServerStartStrategy(String swiHome, String classPath, String debugLevel) {
		super();
		this.swiHome = swiHome;
		this.classPath = classPath;
		this.debugLevel = debugLevel;
	}
	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.ServerStartStrategy#startServer()
	 */
	public Process startServer(int port) {
	    try {
	    boolean isWindoof = System.getProperty("os.name").indexOf("Windows")>-1;
		String dir = isWindoof ? swiHome+"\\bin" 
					: ".";			
		
		String cmdline = "java -classpath "+classPath
				+ " org.cs3.pl.prolog.internal.PrologInterfaceServer " + port;
				
		Debug.debug("Starting server with " + cmdline);
		Debug.debug("dir="+dir);
		Process process =null;
		
			process= Runtime.getRuntime().exec(cmdline, null, new File(dir));
			new _InputStreamPump(process.getErrorStream()).start();
            new _InputStreamPump(process.getInputStream()).start();
            
            
				
		
		 
		while(!Util.probePort(port)){
                 Thread.sleep(50);        
         }
		return process;
	    }
		catch (Throwable e) {
		    Debug.report(e);
		    throw new RuntimeException(e);
        }
	}

}

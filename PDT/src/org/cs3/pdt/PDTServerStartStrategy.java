package org.cs3.pdt;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ServerStartStrategy;

public class PDTServerStartStrategy implements ServerStartStrategy {

		
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
		
		String cmdline = "java -D"+Properties.DEBUG_LEVEL+"="
				+debugLevel
				+" -classpath "+classPath
				+ " org.cs3.pl.prolog.PrologInterfaceServer " + port;
				
		Debug.debug("Starting server with " + cmdline);
		Debug.debug("dir="+dir);
		Process process =null;
		try {
			process= Runtime.getRuntime().exec(cmdline, null, new File(dir));
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

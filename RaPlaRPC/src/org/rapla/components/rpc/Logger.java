/*
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.rapla.components.rpc;

import org.cs3.pl.common.Debug;

/**
 * @author xproot
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class Logger {
	
	private final static String tmpPath = System.getProperty("java.io.tmpdir");
	private final static String lineSep = System.getProperty("line.separator");
	private boolean log = true;
	private boolean debug = false;
	private boolean warning = false;
	private boolean info = false;
	private boolean error = false;
	private final String name;
	public Logger(String name){
		this.name = name;
	}


	
	public Logger getChildLogger(String name){
		return new Logger(name);
	}
	
	public void debug(String msg){
		if (debug)
			Debug.debug(msg);
	}

	public void error(String err, Exception ex){
		Debug.error(err);
		Debug.report(ex);
	    
	}

	public void error(String err){
		Debug.error(err);
	}
	
	
	public void warn(String msg){
		Debug.warning(msg);
	}

	public void info(String msg){
		Debug.info(msg);
	}
	
	public void log(String msg){
		Debug.info(msg);
	}
	
	synchronized public void setLog(boolean log) {
		this.log = log;
	}

	synchronized public boolean isLog() {
		return log;
	}

	synchronized public void setDebug(boolean debug) {
		this.debug = debug;
	}

	synchronized public boolean isDebugEnabled() {
		return debug;
	}

	public void setWaring(boolean warning) {
		this.warning = warning;
	}

	public boolean isWaringEnabled() {
		return warning;
	}

	public void setInfo(boolean info) {
		this.info = info;
	}

	public boolean isInfoEnabled() {
		return info;
	}
	

}

/*
 * Created on 05.02.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.rapla.components.rpc;

import java.io.FileWriter;
import java.io.IOException;

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

	private void write(String str) {
		try {
			FileWriter writer = new FileWriter(tmpPath + java.io.File.separatorChar+ "pdt.socket.communication.log",true);
			writer.write((str + lineSep));
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
//		System.err.println(str);
	}
	
	public Logger getChildLogger(String name){
		return new Logger(name);
	}
	
	public void debug(String msg){
		if (debug)
			write("DEBUG: "+msg);
	}

	public void error(String err, Exception ex){
		if (error)
			write("ERROR: "+err);
		ex.printStackTrace();
	}

	public void error(String err){
		if (error)
			write("ERROR: "+err);
	}
	
	
	public void warn(String msg){
		if (warning)		
		write("WARNING: "+msg);
	}

	public void info(String msg){
		if (info)
			write("INFO: "+msg);
	}
	
	public void log(String msg){
		if (log)
			write(msg);
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

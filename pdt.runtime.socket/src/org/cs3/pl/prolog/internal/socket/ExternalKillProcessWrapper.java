package org.cs3.pl.prolog.internal.socket;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;

//public class ExternalKillProcessWrapper implements ProcessWrapper {
public class ExternalKillProcessWrapper {
	
//	private Process process;	
	private long processId;

//	public ExternalKillProcessWrapper(Process process, String[] strings) {
//		this.process=process;		
//		this.killCommandArray=strings;//Util.split(killCommand," ");
//	}
	
//	public ExternalKillProcessWrapper(Process process, long processId) {
//		this.process=process;		
//		this.processId=processId;
//	}

	public ExternalKillProcessWrapper(long processId) {
//		this.process=process;		
		this.processId=processId;
	}

	
	public void destroy() {
		try{
//			Process p = Runtime.getRuntime().exec(killCommandArray);
//			p.waitFor();
			Util.killRuntimeProcesses(processId);
		}
		catch(Throwable t){
			Debug.report(t);
		}
	}

//	public Process getProcess() {
//		return process;
//		
//	}

}

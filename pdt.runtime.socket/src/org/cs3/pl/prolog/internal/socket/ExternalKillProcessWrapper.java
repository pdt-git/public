package org.cs3.pl.prolog.internal.socket;

import org.cs3.pl.common.Debug;

public class ExternalKillProcessWrapper implements ProcessWrapper {

	private Process process;	
	private String[] killCommandArray;

	public ExternalKillProcessWrapper(Process process, String[] strings) {
		this.process=process;		
		this.killCommandArray=strings;//Util.split(killCommand," ");
	}
/*
	public ExternalKillProcessWrapper(Process process, String[] strings) {
		this.process=process;		
		this.killCommandArray=strings;//Util.split(strings," ");
	}
*/	
	
	public void destroy() {
		try{
			Process p = Runtime.getRuntime().exec(killCommandArray);
			p.waitFor();
		}
		catch(Throwable t){
			Debug.report(t);
		}
	}

	public Process getProcess() {
		return process;
		
	}

}

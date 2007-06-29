package org.cs3.pl.prolog.internal.pifcom;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;

public class ExternalKillProcessWrapper implements ProcessWrapper {

	private Process process;	
	private String[] killCommandArray;

	public ExternalKillProcessWrapper(Process process, String killCommand) {
		this.process=process;		
		this.killCommandArray=Util.split(killCommand," ");
	}
	
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

package org.cs3.jtransformer.internal.natures;

import org.cs3.jtransformer.JTDebug;

/**
 * @author trho
 * 
 * To change this generated comment edit the template variable "typecomment":
 * Window>Preferences>Java>Templates. To enable and disable the creation of type
 * comments go to Window>Preferences>Java>Code Generation.
 */
public class TimeMeasurement
{

	private long ms = 0;

	String name;

	private boolean printTimeMeasuring;

	private int logLevel;

	public TimeMeasurement(String name,int logLevel)
	{
		this.logLevel = logLevel;
		this.name = name;
		ms = System.currentTimeMillis();
		this.printTimeMeasuring = printTimeMeasuring;
		
		JTDebug.write(logLevel,getStartMessage());

	}

	public String getStartMessage() {
		return "start " + name + " ..." ;
	}

//	public void println(String s)
//	{
//		System.out.println(s);
//	}
//
//	public void print(String s)
//	{
//		System.out.print(s);
//	}

	public void logTimeDiff() {
		JTDebug.write(logLevel, "end   " + name + "\t\t"+getTimeDiff() + " sec\n");
	}

	public String getTimeDiff() {
		long diff = getTimeDiffInMillies();
		return diff / 1000 + "." + diff % 1000;
	}

	private long getTimeDiffInMillies() {
		return System.currentTimeMillis() - ms;
	}
	
	
	
	

}

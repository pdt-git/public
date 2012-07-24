/* $LICENSE_MSG$ */

package org.cs3.plunit;

/**
 * @author Tobias Rho
 * 
 */
public class TimeMeasurement
{

	private long ms = 0;

	String name;

	private boolean printTimeMeasuring;

	public TimeMeasurement(String name, boolean printTimeMeasuring)
	{
		this.name = name;
		ms = System.currentTimeMillis();
		this.printTimeMeasuring = printTimeMeasuring;
		if (printTimeMeasuring)
			println("start " + name + " ...");
	}

	public void println(String s)
	{
		System.out.println(s);
	}

	public void print(String s)
	{
		System.out.print(s);
	}

	public long getTimeDiff()
	{
		long diff = System.currentTimeMillis() - ms;
		if (printTimeMeasuring)
		{
			print("end   " + name + "\t\t");
			println(diff / 1000 + "." + diff % 1000 + " sec");
		}
		return diff;
	}

}


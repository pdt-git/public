package org.cs3.timetracker;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

public class Logger {
	
	private TimeTicker ticker;

	public Logger(TimeTicker ticker) {
		this.ticker = ticker;
	}
	
	private String Filename = "test.txt";
	
	//private int lastSeconds = 180;
	
	private int toSeconds(String Minutes, String Seconds)
	{
		return Integer.parseInt(Minutes) * 60 + Integer.parseInt(Seconds);
		
	}
	
	/*
	 * toTime(180) -> "03:00"
	 */
	private String toTime(int seconds)
	{
		String minString = ""+(seconds / 60);
		if ((seconds / 60) < 10) minString = "0" + minString;

		String secString = ""+(seconds % 60);
		if ((seconds % 60) < 10) secString = "0" + secString; 
		
		return minString + ":" + secString;
	}
	
	public String readLog(){
		
		byte[] InputLogByteArray = null; 
		
		File FileObject = new File(Filename);
		try {
			FileInputStream InputStreamObject = new FileInputStream(FileObject);
			
			InputLogByteArray = new byte[InputStreamObject.available()];
			InputStreamObject.read(InputLogByteArray);
			
		}
		catch(IOException e) {
			//e.printStackTrace();
			System.out.println("IO Exception occured, while reading from Logfile.");
			return "";
		}		
				
		return new String(InputLogByteArray);
	}
	
	public String log(String Comment) throws IllegalArgumentException
	{
		File FileObject = new File(Filename);
		String LogString;
		
//		int tempMinutes = Integer.parseInt(Minutes);
//		if ((tempMinutes < 0) || (tempMinutes > 59)) throw new IllegalArgumentException();
//		
//		int tempSeconds = Integer.parseInt(Seconds);
//		if ((tempSeconds < 0) || (tempSeconds > 59)) throw new IllegalArgumentException();
//		
//		if ((Comment.equals("") || (Comment.equals("\n")))) throw new IllegalArgumentException();
		
		LogString = "Recorded ["+toTime(ticker.getLastTimeStamp())+
		  " "+toTime(ticker.getCurrentSeconds())+
		  " "+toTime(ticker.getTimeDifference())+"]"+
		  " "+Comment+
		  "\n";
		try {
			FileOutputStream OutputStreamObject = new FileOutputStream(FileObject, true);
			OutputStreamObject.write(LogString.getBytes());
		}
		catch(IOException e) {
			System.out.println("IO-Exception occured while writing to Logfile.");
		}
		
		//lastSeconds = Integer.parseInt(Minutes) * 60 + Integer.parseInt(Seconds);
		
		ticker.resetTimeStamp();

		return LogString;
	}

}

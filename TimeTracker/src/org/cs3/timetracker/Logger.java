package org.cs3.timetracker;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class Logger {
	
	private String Filename = "test.txt";
	
	private int lastSeconds = 180;
	
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
	
	public String log(String Minutes, String Seconds, String Comment) throws IllegalArgumentException
	{
		File FileObject = new File(Filename);
		String LogString;
		
		int tempMinutes = Integer.parseInt(Minutes);
		if ((tempMinutes < 0) || (tempMinutes > 59)) throw new IllegalArgumentException();
		
		int tempSeconds = Integer.parseInt(Seconds);
		if ((tempSeconds < 0) || (tempSeconds > 59)) throw new IllegalArgumentException();
		
		if ((Comment.equals("") || (Comment.equals("\n")))) throw new IllegalArgumentException();
		
		LogString = "Recorded ["+toTime(lastSeconds)+
		  " "+toTime(toSeconds(Minutes, Seconds))+
		  " "+toTime(lastSeconds - toSeconds(Minutes, Seconds))+"]"+
		  " "+Comment;
		
		try {
			FileOutputStream OutputStreamObject = new FileOutputStream(FileObject);
			OutputStreamObject.write(LogString.getBytes());
		}
		catch(IOException e) {
			System.out.println("IO-Exception occured while writing to Logfile.");
		}
		
		lastSeconds = Integer.parseInt(Minutes) * 60 + Integer.parseInt(Seconds);
		
		return LogString;
	}

}

package org.cs3.timetracker;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;

public class Logger {
	
	private String Filename = "test.txt";
	
	public void Log(String Minutes, String Seconds, String Comment) throws IllegalArgumentException
	{
		File FileObject = new File(Filename);
		String LogString;
		
		int tempMinutes = Integer.parseInt(Minutes);
		if ((tempMinutes < 0) || (tempMinutes > 59)) throw new IllegalArgumentException();
		
		int tempSeconds = Integer.parseInt(Seconds);
		if ((tempSeconds < 0) || (tempSeconds > 59)) throw new IllegalArgumentException();
		
		if ((Comment.equals("") || (Comment.equals("\n")))) throw new IllegalArgumentException();

		LogString = "Recorded ["+Minutes+":"+Seconds+"] "+Comment;
		
		try {
			FileOutputStream OutputStreamObject = new FileOutputStream(FileObject);
			OutputStreamObject.write(LogString.getBytes());
		}
		catch(IOException e) {
			System.out.println("IO-Exception occured while writing to Logfile.");
		}		
	}

}

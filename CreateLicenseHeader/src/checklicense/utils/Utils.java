package checklicense.utils;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class Utils {

	public static String readFileToString(File f) {
		BufferedReader br = null;
		StringBuffer buf = new StringBuffer();
		 
		try {
 
			String sCurrentLine = "";
 
			br = new BufferedReader(new FileReader(f));
 
			while ((sCurrentLine = br.readLine()) != null) {
				buf.append(sCurrentLine);
				buf.append("\n");
			}
 
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				if (br != null)br.close();
			} catch (IOException ex) {
				ex.printStackTrace();
			}
		}
		
		return buf.toString();
	}
	
	public static void writeToFile(File f, String str) {
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(f, false));
			writer.write(str);
			writer.write("\n");
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
}

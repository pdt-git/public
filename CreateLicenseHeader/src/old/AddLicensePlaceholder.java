/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package old;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

public class AddLicensePlaceholder {


	private static final String PROJECT_PATH = "L:/Work/noth/git/repository/pdt";
	
	public void removeOldLicense() {
		String licensePL = readFileToString(new File("L:/work/noth/license/pl.txt"));
		String licenseJava = readFileToString(new File("L:/work/noth/license/java.txt"));
		
		File root = new File(PROJECT_PATH);
		if (root.isDirectory()) {
			removeOldLicense(root, licensePL, licenseJava);
		}
	}
	
	private void removeOldLicense(File f, String... licenses) {
		if (f.isDirectory()) {
			File[] subfiles = f.listFiles();
			for (File sub : subfiles) {
				removeOldLicense(sub, licenses);
			}
		} else if (f.isFile() && hasValidFileExtension(f)) {
			removeLicense(f, licenses);
		}
	}

	private void removeLicense(File f, String... licenses) {
		String oldCode = readFileToString(f);
		boolean replaced  =false;
		
		for (String l : licenses) {
			if (oldCode.contains(l)) {
				String newCode = oldCode.replace(l, "/* $LICENSE_MSG$(ld) */\n");
				writeToFile(f, newCode);
				replaced = true;
				break;
			}
		}
		
		if (!replaced && !oldCode.trim().startsWith("%") && !oldCode.trim().startsWith("/*")) {
			String newCode = "/* $LICENSE_MSG$ */\n\n" + oldCode;
			writeToFile(f, newCode);
		} else if(!replaced) {
			System.out.println("no license in " + f.getPath());
		}
	}


	private void writeToFile(File f, String str) {
		try {
			BufferedWriter writer = new BufferedWriter(new FileWriter(f, false));
			writer.write(str);
			writer.write("\n");
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	private boolean hasValidFileExtension(File f) {
		String filename = f.getName().toLowerCase();
		return filename.endsWith("pl") || filename.endsWith("java");
	}

	
	public String readFileToString(File f) {
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
	
	public static void main(String[] args) {
		AddLicensePlaceholder plc = new AddLicensePlaceholder();
		plc.removeOldLicense();
	}
}

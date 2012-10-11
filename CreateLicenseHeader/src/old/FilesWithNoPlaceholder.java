/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Fabian Noth
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package old;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;

public class FilesWithNoPlaceholder {

	private static final String PROJECT_PATH = "L:/Work/noth/git/repository/pdt";
	
	private void listFiles() {
		listFiles(new File(PROJECT_PATH));
	}
	
	private void listFiles(File f) {
		if (f.isDirectory()) {
			File[] subfiles = f.listFiles();
			for (File sub : subfiles) {
				listFiles(sub);
			}
		} else if (f.isFile() && hasValidFileExtension(f)) {
			checkFile(f);
		}
	}


	private void checkFile(File f) {
		String oldCode = readFileToString(f);
		
		if (!oldCode.startsWith("/* $LICENSE_MSG$")) {
			System.out.println(f.getPath());
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
		FilesWithNoPlaceholder f = new FilesWithNoPlaceholder();
		f.listFiles();
	}

	
}

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
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;

public class CreateLicenseHeader {

	private static final String LICENSE_PREFIX = "/* $LICENSE_MSG$";
	private static final String PROJECT_PATH = "L:\\Work\\noth\\git\\repository\\pdt",
								LICENSE_START = "/*****************************************************************************\n * This file is part of the Prolog Development Tool (PDT)\n * \n",
	                            LICENSE_WWW = " * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start\n",
	                            LICENSE_MAIL = " * Mail: pdt@lists.iai.uni-bonn.de\n",
	                            LICENSE_COPYRIGHT = " * Copyright (C): 2004-2012, CS Dept. III, University of Bonn\n",
	                            LICENSE_EPL = " * \n * All rights reserved. This program is  made available under the terms\n * of the Eclipse Public License v1.0 which accompanies this distribution,\n * and is available at http://www.eclipse.org/legal/epl-v10.html\n",
	                            LICENSE_END = " * \n ****************************************************************************/";
	
	private HashMap<String, String> nameMap = new HashMap<String, String>();
	
	
	private CreateLicenseHeader() {
		nameMap.put("ld", "Lukas Degener");
		nameMap.put("tr", "Tobias Rho");
		nameMap.put("gk", "Günter Kniesel");
		nameMap.put("ab", "Andreas Becker");
		nameMap.put("fn", "Fabian Noth");
		nameMap.put("es", "Eva Stöwe");
		nameMap.put("fm", "Frank Mühlschlegel");
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
	
	public void create() {
		File root = new File(PROJECT_PATH);
		if (root.isDirectory()) {
			create(root);
		}
	}
	
	private void create(File f) {
		if (f.isDirectory()) {
			File[] subfiles = f.listFiles();
			for (File sub : subfiles) {
				create(sub);
			}
		} else if (f.isFile() && hasValidFileExtension(f)) {
			addHeaderToJavaFile(f);
		}
	}


	private void addHeaderToJavaFile(File f) {
		String code = readFileToString(f);
		StringBuffer newCode = new StringBuffer();
		
		if (code.startsWith(LICENSE_PREFIX)) {

			newCode.append(LICENSE_START);

			int startRealCode = code.indexOf("*/") + 2;
			if (code.startsWith(LICENSE_PREFIX + "(")) {
				// with author names
				int endIndex = code.indexOf(")");
				String names = code.substring(LICENSE_PREFIX.length() + 1, endIndex);
				String[] splittedNames = names.split(",");
				if (splittedNames.length > 0) {
					newCode.append(" * Author: ");
					boolean addComma = false;
					for(String name : splittedNames) {
						if (addComma) {
							newCode.append(", ");
						} else {
							addComma = true;
						}
						newCode.append(parseName(name.trim()));
					}
					newCode.append(" (among others)\n");
				}
			}
			newCode.append(LICENSE_WWW);
			newCode.append(LICENSE_MAIL);
			newCode.append(LICENSE_COPYRIGHT);
			newCode.append(LICENSE_EPL);
			newCode.append(LICENSE_END);
			
			newCode.append(code.substring(startRealCode));
			
			writeToFile(f, newCode.toString());
		} else {
			System.out.println("no license header in: " + f.getPath());
		}
	}

	private String parseName(String shortName) {
		return nameMap.get(shortName.trim());
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


	private boolean hasValidFileExtension(File f) {
		String filename = f.getName().toLowerCase();
		return filename.endsWith("pl") || filename.endsWith("java");
	}


	public static void main(String[] args) {
//		CreateLicenseHeader create = new CreateLicenseHeader();
//		create.create();
	}
	
	
}

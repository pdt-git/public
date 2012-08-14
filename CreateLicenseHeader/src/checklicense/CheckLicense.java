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

package checklicense;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import checklicense.utils.Utils;

public class CheckLicense {

	public static final String NO_HEADER = "no header";
	public static final String NO_AUTHOR = "no author";
	public static final String HEADER_WITH_AUTHOR = "header with author";
	
	private String licenseTop;
	private String licenseBottom;
	private String licenseNoAuthor;
	private String authorLine;

	private final HashMap<String, String> authorShortcuts = new HashMap<>();
	
	public CheckLicense() {
		String license = Utils.readFileToString(new File("data\\license.txt"));
		
		licenseNoAuthor = license.replaceAll("\\$\\$\\$.*\\$\\$\\$", "");
		licenseNoAuthor = licenseNoAuthor.replace("\n\n", "\n");
		
		licenseTop = license.substring(0, license.indexOf("$$$"));
		licenseBottom = license.substring(license.lastIndexOf("$$$")+4);
		
		authorLine = license.substring(license.indexOf("$$$") + 3, license.lastIndexOf(" $$$"));
		
		createAuthorList();
	}

	public void createAuthorList() {
		String authors = Utils.readFileToString(new File("data/authors.txt"));
		String[] authorArray = authors.split("\n");
		for (String author : authorArray) {
			String[] split = author.split("\\|");
			if (split.length == 2) {
				authorShortcuts.put(split[0], split[1]);
			}
		}
	}
	
	public HashMap<String, ArrayList<String>> checkFiles(String pathName) {
		File root = new File(pathName);
		if(!root.isDirectory()) {
			final HashMap<String, ArrayList<String>> result = new HashMap<>();
			final ArrayList<String> dummyList = new ArrayList<>();
			dummyList.add("no valid directory");
			result.put(NO_HEADER, dummyList);
			result.put(NO_AUTHOR, dummyList);
			result.put(HEADER_WITH_AUTHOR, dummyList);
			return result;
		}
		final HashMap<String, ArrayList<String>> result = new HashMap<>();
		result.put(NO_HEADER, new ArrayList<String>());
		result.put(NO_AUTHOR, new ArrayList<String>());
		result.put(HEADER_WITH_AUTHOR, new ArrayList<String>());
		checkFiles(root, result);
		return result;
	}
	
	private void checkFiles(File f, HashMap<String, ArrayList<String>> list) {
		if (f.isDirectory()) {
			File[] subfiles = f.listFiles();
			for (File sub : subfiles) {
				checkFiles(sub, list);
			}
		} else if (f.isFile() && hasValidFileExtension(f)) {
			checkFile(f, list);
		}
	}


	private void checkFile(File f, HashMap<String, ArrayList<String>> list) {
		String oldCode = Utils.readFileToString(f);
		
		if (hasNoHeader(oldCode)) {
			list.get(NO_HEADER).add(f.getPath());
		} else if (hasHeaderWithoutAuthor(oldCode)) {
			list.get(NO_AUTHOR).add(f.getPath());
		} else if (hasHeaderWithAuthor(oldCode)) {
			list.get(HEADER_WITH_AUTHOR).add(f.getPath());
		}
	}

	public boolean hasNoHeader(String code) {
		return !code.startsWith(licenseTop) || !code.contains(licenseBottom);
	}
	
	public boolean hasHeaderWithoutAuthor(String code) {
		return code.startsWith(licenseNoAuthor);
	}

	public boolean hasHeaderWithAuthor(String code) {
		return code.startsWith(licenseTop + authorLine) && code.contains(licenseBottom);
	}
	
	private boolean hasValidFileExtension(File f) {
		String filename = f.getName().toLowerCase();
		return filename.endsWith("pl") || filename.endsWith("pro") || filename.endsWith("java");
	}
	
	public void addHeaderToFile(String filePath, String author) {
		File f = new File(filePath);
		String oldContent = Utils.readFileToString(f);
		
		
		StringBuffer newContent = new StringBuffer();
		newContent.append(licenseTop);
		if (!author.isEmpty()) {
			newContent.append(" * Author: ");
			newContent.append(createAuthorString(author));
			newContent.append("\n");
		}
		newContent.append(licenseBottom);
		newContent.append("\n");

		int licenseIndex = oldContent.indexOf(licenseBottom);
		// remove license if it is already there
		if (licenseIndex > -1) {
			String filteredContent = oldContent.substring(licenseIndex + licenseBottom.length() + 1);
			newContent.append(filteredContent);
		} else {
			newContent.append(oldContent);
		}
		
		Utils.writeToFile(f, newContent.toString());
	}

	private String createAuthorString(String author) {
		StringBuffer result = new StringBuffer();
		
		String[] splitted = author.split(",");
		
		for(int i=0; i<splitted.length; i++) {
			String name = splitted[i].trim();
			if (i > 0) {
				result.append(", ");
			}
			if (authorShortcuts.containsKey(name.toLowerCase())) {
				result.append(authorShortcuts.get(name.toLowerCase()));
			} else {
				result.append(name);
			}
			
		}
		
		return result.toString();
	}

	public void addAuthorToFile(String filePath, String newAuthors) {
		File f = new File(filePath);
		String oldContent = Utils.readFileToString(f);
		
		if (hasNoHeader(oldContent) || hasHeaderWithoutAuthor(oldContent)) {
			// if there are no authors, adding and seting an author is the same
			addHeaderToFile(filePath, newAuthors);
		} else {
			// parse authors from file
			int authorLineIndex = oldContent.indexOf(authorLine);
			String oldAuthors = oldContent.substring(authorLineIndex + authorLine.length(), oldContent.indexOf("\n", authorLineIndex));
			String authors = oldAuthors + ", " + newAuthors;
			String[] splittedAuthors = authors.split(",");
			ArrayList<String> authorsList = new ArrayList<>();
			// add new authors (if they are not in the header yet)
			for (String name : splittedAuthors) {
				name = name.trim();
				if (authorShortcuts.containsKey(name)) {
					name = authorShortcuts.get(name);
				}
				
				if (!authorsList.contains(name)) {
					authorsList.add(name);
				}
			}

			// save file
			StringBuffer finalAuthorList = new StringBuffer();
			boolean first = true;
			for (String s : authorsList) {
				if (first) {
					first = false;
				} else {
					finalAuthorList.append(", ");
				}
				finalAuthorList.append(s);
			}
			addHeaderToFile(filePath, finalAuthorList.toString());
			
		}
	}
	
}

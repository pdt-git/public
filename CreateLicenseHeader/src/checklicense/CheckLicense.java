package checklicense;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import checklicense.utils.Utils;

public class CheckLicense {

	public static final String NO_HEADER = "no header";
	public static final String NO_AUTHOR = "no author";
	
	private String licenseTop;
	private String license;
	private String licenseComplete;
	
	public CheckLicense() {
		licenseTop = Utils.readFileToString(new File("data/license_top.txt"));
		license = Utils.readFileToString(new File("data/license.txt"));
		licenseComplete = licenseTop + license;
	}
	
	public HashMap<String, ArrayList<String>> checkFiles(String pathName) {
		File root = new File(pathName);
		if(!root.isDirectory()) {
			final HashMap<String, ArrayList<String>> result = new HashMap<>();
			final ArrayList<String> dummyList = new ArrayList<>();
			dummyList.add("no valid directory");
			result.put(NO_HEADER, dummyList);
			result.put(NO_AUTHOR, dummyList);
			return result;
		}
		final HashMap<String, ArrayList<String>> result = new HashMap<>();
		result.put(NO_HEADER, new ArrayList<String>());
		result.put(NO_AUTHOR, new ArrayList<String>());
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
		
		if (!hasHeader(oldCode)) {
			list.get(NO_HEADER).add(f.getPath());
		} else if(hasHeaderWithoutAuthor(oldCode)) {
			list.get(NO_AUTHOR).add(f.getPath());
		}
	}

	public boolean hasHeaderWithoutAuthor(String oldCode) {
		return oldCode.startsWith(licenseComplete);
	}

	public boolean hasHeader(String oldCode) {
		return oldCode.startsWith(licenseTop) && oldCode.contains(license);
	}
	
	private boolean hasValidFileExtension(File f) {
		String filename = f.getName().toLowerCase();
		return filename.endsWith("pl") || filename.endsWith("java");
	}

	public void addHeaderToFile(String filePath, String author) {
		File f = new File(filePath);
		String oldContent = Utils.readFileToString(f);
		
		if (hasHeader(oldContent) && !hasHeaderWithoutAuthor(oldContent)) {
			// there is already a header with an author, do nothing
			return;
		}
		
		if (hasHeader(oldContent) && author.isEmpty()) {
			// there already is a simple header and no author, do nothing
			return;
		}
		
		StringBuffer newContent = new StringBuffer();
		newContent.append(licenseTop);
		if (!author.isEmpty()) {
			newContent.append(" * Author: " + author + "\n");
		}
		newContent.append(license);
		newContent.append("\n");
		// remove license if it is already there
		// this is the case if we just want to add an author
		newContent.append(oldContent.replace(licenseComplete + "\n", ""));
		Utils.writeToFile(f, newContent.toString());
	}
	
	

}

package checklicense;

import java.io.File;
import java.util.ArrayList;

import checklicense.utils.Utils;

public class CheckLicense {

	String licenseTop;
	String license;
	
	public CheckLicense() {
		licenseTop = Utils.readFileToString(new File("data/license_top.txt"));
		license = Utils.readFileToString(new File("data/license.txt"));
	}
	
	public ArrayList<String> checkFiles(String pathName) {
		File root = new File(pathName);
		if(!root.isDirectory()) {
			final ArrayList<String> result = new ArrayList<String>();
			result.add("no valid directory");
			return result;
		}
		final ArrayList<String> result = new ArrayList<String>();
		checkFiles(root, result);
		return result;
	}
	
	private void checkFiles(File f, ArrayList<String> list) {
		if (f.isDirectory()) {
			File[] subfiles = f.listFiles();
			for (File sub : subfiles) {
				checkFiles(sub, list);
			}
		} else if (f.isFile() && hasValidFileExtension(f)) {
			checkFile(f, list);
		}
	}


	private void checkFile(File f, ArrayList<String> list) {
		String oldCode = Utils.readFileToString(f);
		
		if (!oldCode.startsWith(licenseTop) || !oldCode.contains(license)) {
			list.add(f.getPath());
		}
	}
	
	private boolean hasValidFileExtension(File f) {
		String filename = f.getName().toLowerCase();
		return filename.endsWith("pl") || filename.endsWith("java");
	}

	public void addHeaderToFile(String s) {
		File f = new File(s);
		String newContent = licenseTop + license + "\n" + Utils.readFileToString(f);
		Utils.writeToFile(f, newContent);
	}
	
	

}

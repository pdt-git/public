/*
 * Created on 18.06.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.fileops;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.PDTPlugin;
import org.cs3.pl.exceptions.ExceptionHandler;
import org.cs3.pl.prolog.IMetaInfoProvider;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologManager;

/**
 * @author elwe-adm
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class MetaDataManager {
	
	/**
	 * The meta data subdirectory for documentation.
	 */
	
	static public final String DOC = "doc" + File.separatorChar;
	private String extension;
	private static Hashtable managers = new Hashtable();

//	public static synchronized MetaDataManager getInstance(String dir) throws IOException {
//		MetaDataManager manager = (PrologMetaDataManager)managers.get(dir);
//		if(manager == null) {
//			manager = new MetaDataManager(dir);
//			managers.put(dir,manager);
//			// StS: Regularily fails if old files are present. What do we need it for?
//			// manager.loadMetaData(dir);
//		}
//		return manager;
//	}

	
	protected String getMetaDataFileLocation(String filename){
		if(filename.charAt(0) == '/')
			filename = filename.substring(1);
		if ('/' != File.separatorChar)
			filename = filename.replace('/',File.separatorChar);
		if(filename.charAt(0) == File.separatorChar)
			filename = filename.substring(1);
		return getStateLocation() + filename;
	}
		
	/**
	 * Returns the state location guaranteeing that it is postfixed with separator.
	 * 
	 * @return
	 */
	protected String getStateLocation() {
		String loc = PDTPlugin.getDefault().getStateLocation().toOSString();
		if(loc.charAt(loc.length()-1) != File.separatorChar)
			loc += File.separatorChar;
		return loc + dir;
	}

	/**
	 * @param filename
	 * @return
	 */
	public BufferedWriter getMetaDataWriter(String filename) throws IOException {
		String location = getMetaDataFileLocation(getFileNameWithNewExtension(filename));
		File file = new File(location);
		if(!(file.getParentFile().exists()))
				file.getParentFile().mkdirs();
		return new BufferedWriter(new FileWriter(file)); 
	}

	
	private final String dir;
	
	private static final boolean debug = true;
	
	/**
	 * 
	 * @param dir
	 */
	
	MetaDataManager(String dir, String ext) {
		this.dir = dir;
		this.extension = ext;
		File file = new File(getStateLocation());
		if(!file.exists())
			file.mkdirs();
		
		
	}

	/**
	 * @param path
	 * @return
	 */
	protected String getFileNameWithNewExtension(String path) {
		int indexOfDot = path.lastIndexOf(".");
		
		path = path.replace('$', '_');
		
		/* we could have a raw class name, the dread forth case ;) */
		if (indexOfDot != -1)
			path = path.substring(0, indexOfDot);
		
		return path + "."+ extension;
	}
	
	public void deleteAll() {
		File handle = new File(getStateLocation());
		
		if  (handle.isDirectory())
			deleteChildren(handle);
		
		handle.delete();
	}

	/**
	 * deletes the children of a directory. a "deep delete"
	 * @param handle
	 */
	private void deleteChildren(File handle) {
		File [] members = handle.listFiles();
		
		for (int i = 0; i < members.length; i++) {
			if (members[i].isDirectory())
				deleteChildren(members[i]);
			
			members[i].delete();
		}
	}

	/**
	 * @param filename
	 * @return
	 */
	public String getFullPath(String filename) {
		return getStateLocation()+getFileNameWithNewExtension(filename);
	}

	/**
	 * Checks if the file filename exists in the meta data repository.
	 * 
	 * @param filename
	 * @return
	 */
	public boolean exists(String filename) {
		return (new File(getStateLocation()+getFileNameWithNewExtension(filename))).exists();
	}

}

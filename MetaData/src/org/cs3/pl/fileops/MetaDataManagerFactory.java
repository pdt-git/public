/*
 * Created on 18.06.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.fileops;

import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.prolog.IPrologInterface;

/**
 * @author elwe-adm
 *
 * To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
public class MetaDataManagerFactory {

	private static Hashtable managers = new Hashtable();
	
	/**
	 * Returns the MetaDataManager instance for the directory dir.
	 * For each file in this directory dir the file extension will be replaced by 
	 * ext.
	 * 
	 * @param dir the meta data directory
	 * @param ext the new file extension.
	 * @return
	 */
	
	static public MetaDataManager getMetaDataManager(String dir, String ext) {
		MetaDataManager manager = (MetaDataManager)managers.get(dir);
		if(manager == null) {
			manager = new MetaDataManager(dir,ext);
			managers.put(dir,manager);
		}	
		return manager;
	}

	/**
	 * TODO: 
	 * 
	 * @param dir
	 * @return
	 * @throws IOException
	 */
	static public PrologMetaDataManager getPrologMetaDataManager(IPrologInterface client, String dir) throws IOException {
			PrologMetaDataManager manager = new PrologMetaDataManager(client, dir);
			managers.put(dir,manager);
			if (dir == PrologMetaDataManager.MODEL)
				manager.reloadMetaData();
		return manager;
	}


}

/* $LICENSE_MSG$(ld) */

/*
 */
package org.cs3.prolog.common;

import java.io.File;

import org.cs3.prolog.common.logging.Debug;

/**
 * Canonical implementation.
 * This implementation uses a given directory URI as root
 * of its resource tree. By default it uses the user home directory.
 */
public class DefaultResourceFileLocator implements ResourceFileLocator {
    String root;
     
     public DefaultResourceFileLocator(File root) {
     	String rootPath = root.getAbsolutePath();
         try {
             this.root=rootPath.endsWith(File.separator)? rootPath:rootPath+File.separator;
         } catch (Exception e) {
             Debug.report(e);
             throw new RuntimeException(e.getMessage());
         }
     }
     
    @Override
	public ResourceFileLocator subLocator(String subdir){
        return new DefaultResourceFileLocator(resolve(subdir));
    }
    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.common.ResourceLocator#resolve(java.lang.String)
     */
    @Override
	public File resolve(String rel) {
        File resolved = new File(root+ rel);
        return resolved;
    }    
}


/*
 */
package org.cs3.pl.common;

import java.io.File;
import java.io.InputStream;

/**
 * resolves virtual  files or directory names.
 * <p>
 * Intended to enable flexible management resources in situations
 * where it is not feasable to abstract from the filesystem and
 * use the jdk's resource api.
 * <p>
 * Concrete motivation: We want to bootstrap the prolog system.
 * Filesystem seems to be the most simple and relyable way to supply the
 * server side (prolog) implementation of the prolog interface. 
 *  <p>
 *  Alternative: Should be possible to use tcp streams for this.
 *  Might be an elegant alternative, but atm, i do not see that elegance would
 *  justify the additional complexity.
 *  <p>
 *  comment: i would apreciate another solution. i cannot help feeling that this is
 *  somewaht redundant.
 *  
 *   --lu
 *  
 */
public interface ResourceFileLocator {
    /**
     * Resolve a relative resource name to an abstract File.
     * <p>
     * 
     * @return a File that might not exist yet.
     * @param rel the resource name to be resolved.
     */
    public File resolve(String rel);
    
    
    public ResourceFileLocator subLocator(String subdir);
  
}

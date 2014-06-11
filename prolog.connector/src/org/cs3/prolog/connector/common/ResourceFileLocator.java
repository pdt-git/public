/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.prolog.connector.common;

import java.io.File;

/**
 * resolves virtual  files or directory names.
 * <p>
 * Intended to enable flexible management of resources in situations
 * where it is not feasable to abstract from the filesystem (e.g. by 
 * using the jdk's resource api.)
 * <p>
 * RFLs resolve relative path strings to File instances. (see resolve(String)). 
 * instead of resolving a path string, RFLs can also create so-called
 * sub-locators for that path. .   
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
    
    /**
     * create a sub locator for a relative path.
     * <p>
     * The sublocator should behave exactly as this locator would behave if
     * each path passed as an argument to the resolve or subLocator method was 
     * prefixed with the <code>subdir</code> path.
     * @param subdir
     * @return the sublocator
     */
    public ResourceFileLocator subLocator(String subdir);
  
}



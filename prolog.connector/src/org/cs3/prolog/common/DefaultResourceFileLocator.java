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
    
    /**
     * 
     * @see org.cs3.prolog.common.ResourceFileLocator#resolve(java.lang.String)
     */
    @Override
	public File resolve(String rel) {
        File resolved = new File(root+ rel);
        return resolved;
    }    
}



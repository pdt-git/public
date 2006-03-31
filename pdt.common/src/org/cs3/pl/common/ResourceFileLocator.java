/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.common;

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

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
package org.cs3.pl.prolog;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;


public abstract class PrologInterfaceFactory {
   
    // public final static String DEFAULT="org.cs3.pifcom.Factory";
    public final static String DEFAULT="org.cs3.pl.prolog.internal.socket.Factory";
    public final static String PIFCOM="org.cs3.pifcom.Factory";

    
    private ResourceFileLocator locator= new DefaultResourceFileLocator().subLocator(".PrologInterface");

	private PrologLibraryManager libraryManager;

    public static PrologInterfaceFactory newInstance(){
    	
        return newInstance(System.getProperty(PrologRuntime.PREF_PIF_IMPLEMENTATION,DEFAULT));
    }
    
    public static PrologInterfaceFactory newInstance(String fqn) {
        try{
            Class impl = Class.forName(fqn);
        
        if(!PrologInterfaceFactory.class.isAssignableFrom(impl)){
            throw new IllegalArgumentException("not a valid factory class");
        }
        return (PrologInterfaceFactory) impl.newInstance();
        }
        catch(Throwable t){
            Debug.rethrow(t);
            return null;
        }
    }
    
    public abstract PrologInterface create();
    public abstract PrologInterface create(String name);
    
  //  public abstract Option[] getOptions();
    
    public void setResourceLocator(ResourceFileLocator locator){
		this.locator=locator;
    }
    
    public ResourceFileLocator getResourceLocator(){
        return locator;
    }

    public void setLibraryManager(PrologLibraryManager mgr){
    	this.libraryManager=mgr;
    }
    
    public PrologLibraryManager getLibraryManager(){
    	return this.libraryManager;
    }
    
    public File ensureInstalled(String res, Class clazz){
        File f = getResourceLocator().resolve(res);
        
        if(f.exists()){
            f.delete();
        }
        if (!f.exists()){
            f.getParentFile().mkdirs();
            try {
                BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(f));
                InputStream in = clazz.getResourceAsStream(res);
                Util.copy(in,out);
                in.close();
                out.close();            
            } catch (IOException e) {
                Debug.rethrow(e);
            }
        }
        return f;
    }
    
    protected String guessBootDirectory() {
		return null;
	}

    public String guessFileSearchPath(String libraryId) {
		PrologLibraryManager mgr = getLibraryManager();
		if(mgr==null){
			return null;
		}
		PrologLibrary lib= mgr.resolveLibrary(libraryId);
		if(lib==null){
			return null;
		}
		return "library="+lib.getPath();
	}
}

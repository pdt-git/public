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
package org.cs3.pl.prolog.internal.xml;

import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.DefaultConsultService;
import org.cs3.pl.prolog.internal.ReusablePool;

/**
 */
public class XMLPrologInterface extends AbstractPrologInterface {

    private ReusablePool pool = null;
    
    private ResourceFileLocator locator;

    private PrologInterfaceFactory factory;

    public static final String EXECUTABLE = "pif.executable";
    private String executable=null;

    public static final String PORT = "pif.port";

    private int port=12345;

    

    /**
     *  
     */
    public XMLPrologInterface(PrologInterfaceFactory factory) {
        this.factory=factory;
        this.locator=factory.getResourceLocator();
        pool=new ReusablePool( );
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getSession_impl()
     */
    public PrologSession getSession_impl() throws Throwable {
       XMLClient client = (XMLClient) pool.findInstance(XMLClient.class);
       if(client==null){
           client = new XMLClient(Integer.parseInt(getOption(PORT)));
           client.connect();
       }
        return new XMLSession(pool,client,this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterface#getConsultService(java.lang.String)
     */
    public ConsultService getConsultService(String prefix) {
        return new DefaultConsultService(this, locator.subLocator(prefix));
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.PrologInterface#getFactory()
     */
    public PrologInterfaceFactory getFactory() {
        return factory;
    }

    /**
     * @return Returns the locator.
     */
    public ResourceFileLocator getLocator() {
        return locator;
    }

    /**
     * @param locator
     *                  The locator to set.
     */
    public void setLocator(ResourceFileLocator locator) {
        this.locator = locator;
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologInterface#getOption(java.lang.String)
     */
    public String getOption(String opt) {
       if(PORT.equals(opt)){
           return ""+port;
       }
       if(EXECUTABLE.equals(opt)){
        return executable;
       }
        return super.getOption(opt);
    }
    /* (non-Javadoc)
     * @see org.cs3.pl.prolog.PrologInterface#setOption(java.lang.String, java.lang.String)
     */
    public void setOption(String opt, String value) {
        if(PORT.equals(opt)){
           port=Integer.parseInt(value);
        }
        else if(EXECUTABLE.equals(opt)){
            executable=value;
        }
        else{
            super.setOption(opt, value);
        }
    }
}

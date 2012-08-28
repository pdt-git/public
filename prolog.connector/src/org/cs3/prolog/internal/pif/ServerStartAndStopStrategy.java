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

package org.cs3.prolog.internal.pif;

import org.cs3.prolog.pif.PrologInterface;



/**
 * A pluggable strategy for starting and stopping a PIF server.
 * 
 * Since the Prolog System is living in its own process and starting 
 * and stopping this "server" process is dependent on a lot of issues 
 *  - Do I run as an eclipse-plugin?
 *  - Do I run on Windows? ... )
 * this interface abstracts the common behaviour.    
 *  
 */
public interface ServerStartAndStopStrategy {
    /**
     * starts the server process, returning its process.
     * 
     * @return the server process, if available, or null.
     * @param pif the PrologInterface for which the server part should be started.
     */
    public Process startServer(PrologInterface pif);

    /**
     * stop the server process
     * 
     * @param pif the IPrologInterface for which the server should be stopped.
     * @throws Throwable 
     */
    public void stopServer(PrologInterface pif);

    /**
     * @param interface1
     * @return
     */
    public boolean isRunning(PrologInterface pif);
}



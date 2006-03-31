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

package org.cs3.pl.prolog;


/**
 * A pluggable strategy for starting and stopping a PIF server.
 * 
 * Sincethe Prolog System is living in its own process for 
 * all the implemntations we tried until now, and starting 
 * and stopping this "server" process tends to be dependent
 * on a lot of enviremental issues (Do i run as a eclipse plugin?
 * Am i running on a windoze box? ... ), I decided to move 
 * some of this problems into a separate pluggable object.   
 *  
 */
public interface ServerStartAndStopStrategy {
    /**
     * starts the server process, returning its process.
     * 
     * @return the server process, if available, or null.
     * @param pif
     *                    the PrologInterface for which the server part should be
     *                    started.
     */
    public Process startServer(PrologInterface pif);

    /**
     * stop the server process
     * 
     * @param pif
     *                    the IPrologInterface for which the server should be stopped.
     * @param now
     *                    Normaly we should allow the server process to terminate
     *                    gracefully. if this flag is set, the implementation should
     *                    forget all politeness and "simply do it". This is ment as a
     *                    hint. Wether this flag has any effect depends on the concrete
     *                    implementation.
     */
    public void stopServer(PrologInterface pif, boolean now);

    /**
     * @param interface1
     * @return
     */
    public boolean isRunning(PrologInterface pif);
}

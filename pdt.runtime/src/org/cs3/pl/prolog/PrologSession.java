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

import java.util.List;
import java.util.Map;

public interface PrologSession extends Disposable{

	/**
	 * retrieve the PrologInterface that created this session.	
	 */
	public PrologInterface getPrologInterface();
	

    /**
     * begins a query on the Prolog system. If there was an active query
     * running, further results lost, so calls to next() will return results of
     * this query. The resultant Hashtable contains keys equal to the unbound
     * variables in the query, and values equal to their binding. If null is
     * returned, no bindings could satisfy the query ("no"). If there were no
     * unbound variables, an empty Hashtable is returned to signify "yes".
     * <p>
     * <b>please note: </b>
     * <p>
     * It seems that interactive queries imply quiet a deal of extra complexity
     * in all the implementations i tried so far. They are also significantly
     * slower due to the added "synchronisation points" and the resulting io
     * overhead. At the moment i cannot think of any scenario that would require
     * the interactive concept anyway, so i would like to get rid of it asap.
     * 
     * @deprecated use queryAll or queryOnce instead
     * 
     * @return a hashtable, containing the bindings generated.
     * @param query
     *                    a prolog query
     * @throws IllegalStateException
     *                    the session is disposed
     * @throws PrologException
     *                    an abnormal condition was detected
     */

    public Map query(String query) throws PrologException,PrologInterfaceException;

    public Map<String,Object> queryOnce(String query) throws PrologException,PrologInterfaceException;

    public List<Map<String,Object>> queryAll(String query) throws PrologException,PrologInterfaceException;

    /**
     * returns the next set of Bindings satisfying the last query.
     * 
     * <b>please note: </b>
     * <p>
     * It seems that interactive queries imply quiet a deal of extra complexity
     * in all the implementations i tried so far. They are also significantly
     * slower due to the added "synchronisation points" and the resulting io
     * overhead. At the moment i cannot think of any scenario that would require
     * the interactive concept anyway, so i would like to get rid of it asap.
     * 
     * @deprecated use queryAll or queryOnce instead
     * @return another set of Bindings
     * @throws IllegalStateException
     *                    the session is disposed
     * @throws PrologException
     *                    an IO Error occured .
     */

    public Map next() throws PrologException,PrologInterfaceException;

    /**
     * explicitly ends the last query, discarding further results if any
     * existed. If no query was active, this is a noop. <b>please note: </b>
     * <p>
     * It seems that interactive queries imply quiet a deal of extra complexity
     * in all the implementations i tried so far. They are also significantly
     * slower due to the added "synchronisation points" and the resulting io
     * overhead. At the moment i cannot think of any scenario that would require
     * the interactive concept anyway, so i would like to get rid of it asap.
     * 
     * @deprecated use queryAll or queryOnce instead
     * @throws IllegalStateException
     *                    the session is disposed
     * @throws PrologException
     *                    a lower-level failure has occured while killing the query.
     */

    public void endQuery() throws PrologException,PrologInterfaceException;

    public String getProcessorThreadAlias() throws PrologInterfaceException;
   

}

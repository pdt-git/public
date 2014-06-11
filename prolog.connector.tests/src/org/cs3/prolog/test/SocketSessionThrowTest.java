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

package org.cs3.prolog.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

/**
 * @author terra
 */
public class SocketSessionThrowTest extends TestCase {
	private PrologInterface pif;

    @Override
	protected void setUp() throws Exception {
      pif = Connector.newPrologInterface();
      pif.start();
    }
    
    @Override
	protected void tearDown() throws Exception {
        pif.stop();
    }
	
	/**
	 * http://roots.iai.uni-bonn.de/jira/browse/PDT-10
	 * @throws PrologException
	 * @throws PrologInterfaceException 
	 */
	public void testThrow() throws PrologException, PrologInterfaceException{
		PrologSession ss = pif.getSession();
		try  {
		ss.queryOnce("throw(A)");
		} catch(Exception ex){
			System.out.println("");
		}
		ss.dispose();
	}}



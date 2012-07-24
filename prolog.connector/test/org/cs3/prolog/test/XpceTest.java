/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
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

import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;

public class XpceTest extends TestCase {
	public void testXpce() throws  PrologInterfaceException {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
		PrologInterface plInterface = PrologRuntimePlugin.getDefault().newPrologInterface();
		plInterface.start();
		try{			
			plInterface.getSession().queryOnce("help");
		}catch(Exception pissnelke){
			Debug.report(pissnelke);
			fail();
		}

	}
	
	public void _testDifferent() throws Throwable{
		String home=System.getProperty("user.home");
		Runtime.getRuntime().exec(new String[]{"/usr/X11R6/bin/xterm"},new String[]{"DISPLAY=:0.0","HOME="+home});
		System.out.println("success");
		
	}
}



/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Tobias Rho, Lukas Degener, Andreas Becker, Fabian Noth
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
import org.cs3.prolog.lifecycle.LifeCycleHook;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

public class LifeCycleHookTest extends TestCase {


	private class MyHook implements LifeCycleHook{

//		private int lateInit;
//		private int onError;
//		private int setData;
		private int afterInit;
		private int beforeShutdown;
		private int onInit;

		@Override
		public void lateInit(PrologInterface pif) {
			Debug.debug("lateInit");
//			lateInit++;
			
		}

		@Override
		public void onError(PrologInterface pif) {
			Debug.debug("onError");
//			onError++;
			
		}

		@Override
		public void setData(Object data) {
			Debug.debug("setData");
//			setData++;
			
		}

		@Override
		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
			Debug.debug("afterInit");
			afterInit++;
			
		}

		@Override
		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologInterfaceException {
			Debug.debug("beforeShutdown");
			beforeShutdown++;
			
		}

		@Override
		public void onInit(PrologInterface pif, PrologSession initSession)
				throws PrologInterfaceException {
			Debug.debug("onInit");
			onInit++;
			
		}
		
	}
	
	
	private PrologInterface pif;

	@Override
	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
//		this.pif=(PrologInterface) PrologInterfaceFactory.newInstance().create();
		this.pif = PrologRuntimePlugin.getDefault().newPrologInterface();
		
	}

	public void testPDT_295_00() throws Exception{
		MyHook X = new MyHook();
		pif.addLifeCycleHook(X, "X", new String[0]);
		pif.getSession(PrologInterface.NONE).dispose();
		pif.stop();
		assertEquals(1,X.onInit);
		assertEquals(1,X.afterInit);
		assertEquals(1,X.beforeShutdown);
		/*
		 * -Dpif.file_search_path="library=/home/lukas/workspace/pdt.runtime.pifcom/library/pifcom"
-Dpif.implementation="org.cs3.pifcom.Factory"
		 */
		
	}
	
	public void testPDT_295_01() throws Exception{
		MyHook X = new MyHook();
		pif.addLifeCycleHook(X, "X", new String[0]);
		pif.removeLifeCycleHook(X,"X");
		pif.addLifeCycleHook(X, "X", new String[0]);
		pif.getSession(PrologInterface.NONE).dispose();
		pif.stop();
		assertEquals(1,X.onInit);
		assertEquals(1,X.afterInit);
		assertEquals(1,X.beforeShutdown);
		/*
		 * -Dpif.file_search_path="library=/home/lukas/workspace/pdt.runtime.pifcom/library/pifcom"
-Dpif.implementation="org.cs3.pifcom.Factory"
		 */
		
	}
}



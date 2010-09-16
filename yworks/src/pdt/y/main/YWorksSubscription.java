/*****************************************************************************
 * This file is part of the JTransformer Framework
 * 
 * Author: Tobias Rho (among others) 
 * E-mail: rho@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/jtransformer
 * Copyright (C): 2004-2008, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/
package pdt.y.main;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

public class YWorksSubscription extends DefaultSubscription implements
			LifeCycleHook {

		private static final String YWORKS_BOOTSTRAPCONTRIBUTION_KEY = "yworks.contribution.key";
		private static List<String> demandedBootstrapContributionsKeys = new ArrayList<String>();
		
		static {
			demandedBootstrapContributionsKeys.add(YWORKS_BOOTSTRAPCONTRIBUTION_KEY);
		}

		/**
		 * still needed?
		 */
		Object configureMonitor = new Object();
		
//		private PrologInterface pif;

//		static Object toBeBuiltMonitor = new Object();
		
		private YWorksSubscription(String id, String pifID,
				String descritpion, String name) {
			super(id, pifID, descritpion, name, Activator.PLUGIN_ID, true);
			setPersistent(false);

		}

		

//		public boolean isPrologInterfaceUp() {
//			if(pif == null) {
//				return false;
//			}
//			return pif.isUp();
//		}


		/**
		 * Use this method to instantiate a YWorksSubscription. 
		 * @param project
		 * @param pifID
		 * @return
		 */
		public static YWorksSubscription newInstance(String pifID) {
			YWorksSubscription subscription = new YWorksSubscription(
					"YWorksSubscription",
					pifID,
					"Subscription of YWorksDemo", 
					"YWorksDemo"
					);
			return subscription;
		}

		
		@Override
		public List<String> getBootstrapConstributionKeys() {
			return demandedBootstrapContributionsKeys;
		}

		@Override
		public void lateInit(PrologInterface pif) {
		}

		@Override
		public void onError(PrologInterface pif) {
		}

		@Override
		public void setData(Object data) {
		}



		@Override
		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
		}


		@Override
		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologInterfaceException {
		}


		@Override
		public void onInit(PrologInterface pif, PrologSession initSession)
				throws PrologInterfaceException {
		}

	}
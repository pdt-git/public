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

package org.cs3.pdt.connector.registry;

import java.util.EventObject;

import org.cs3.pdt.connector.subscription.Subscription;
import org.cs3.prolog.connector.process.PrologProcess;

public class PrologProcessRegistryEvent extends EventObject {

	private static final long serialVersionUID = 1L;

	public PrologProcess pif = null;

	public String key = null;

	public Subscription subscription = null;

	public PrologProcessRegistryEvent(Object source, 
			PrologProcess pif,
			String key, 
			Subscription subscription) {
		super(source);
		this.pif = pif;
		this.key = key;
		this.subscription = subscription;
	}

	public PrologProcessRegistryEvent(PrologProcessRegistry reg,
			Subscription subscription) {
		super(reg);
		this.key = subscription.getPifKey();
		this.subscription = subscription;
		this.pif = reg.getPrologProcess(key);
	}

	
	public PrologProcessRegistryEvent(PrologProcessRegistry reg,
			String key) {
		super(reg);
		this.key = key;
		this.pif = reg.getPrologProcess(key);
	}
}



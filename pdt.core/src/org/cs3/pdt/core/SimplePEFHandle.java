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

package org.cs3.pdt.core;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.Platform;

public class SimplePEFHandle implements PEFHandle {

	private String id;
	private PrologInterface pif;

	@Override
	public String getId() {

		return id;
	}

	public SimplePEFHandle(PrologInterface pif, String id) {
		super();
		this.pif = pif;
		this.id = id;
	}

	@Override
	public PrologInterface getPrologInterface() {

		return pif;
	}

	@SuppressWarnings("rawtypes")
	@Override
	public Object getAdapter(Class adapter) {
		return Platform.getAdapterManager().getAdapter(this, adapter);
	}

}


